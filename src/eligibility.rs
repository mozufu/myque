use crate::model::{Config, Status, Task};
use crate::store::{StoreError, StoredTask, TaskStore};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SkipReason {
    NotReady { status: Status },
    AutoDispatchDisabled,
    TaskNotOptedIn,
    MissingAllowedLabel { allowed: Vec<String> },
    BlockedLabel { label: String },
    MaxAttemptsReached { attempts: u32, max_attempts: u32 },
    UnknownDependency { dependency: String },
    WaitingOnDependency { dependency: String, status: Status },
    MissingSection { section: &'static str },
    EmptyAcceptance,
    UnknownAgent { agent: String },
    UnknownBackend { backend: String },
    BackendRejected { reason: String },
    ConcurrencyLimit { max_parallel: u32, running: usize },
    DependencyCycle { cycle: Vec<String> },
}

#[derive(Debug, Clone)]
pub struct EligibilityDecision {
    pub task_id: String,
    pub eligible: bool,
    pub reasons: Vec<SkipReason>,
}

#[derive(Debug, Clone)]
pub struct DispatchPlan {
    pub decisions: Vec<EligibilityDecision>,
    pub selected: Vec<String>,
    pub running: usize,
    pub available_slots: usize,
}

impl DispatchPlan {
    pub fn eligible(&self) -> impl Iterator<Item = &EligibilityDecision> {
        self.decisions.iter().filter(|decision| decision.eligible)
    }

    pub fn skipped(&self) -> impl Iterator<Item = &EligibilityDecision> {
        self.decisions.iter().filter(|decision| !decision.eligible)
    }

    pub fn is_selected(&self, task_id: &str) -> bool {
        self.selected.iter().any(|id| id == task_id)
    }
}

#[derive(Debug, Clone)]
pub struct DependencyGraph {
    dependencies: HashMap<String, Vec<String>>,
}

impl DependencyGraph {
    pub fn build<'a>(tasks: impl IntoIterator<Item = &'a Task>) -> Result<Self, Vec<SkipReason>> {
        let mut ids = HashSet::new();
        let mut dependencies = HashMap::new();
        for task in tasks {
            ids.insert(task.id.clone());
            dependencies.insert(task.id.clone(), task.depends_on.clone());
        }

        let mut errors = Vec::new();
        for (task_id, deps) in &dependencies {
            for dep in deps {
                if !ids.contains(dep) {
                    errors.push(SkipReason::UnknownDependency {
                        dependency: format!("{task_id}->{dep}"),
                    });
                }
            }
        }
        errors.extend(
            detect_cycles(&dependencies)
                .into_iter()
                .map(|cycle| SkipReason::DependencyCycle { cycle }),
        );

        if errors.is_empty() {
            Ok(Self { dependencies })
        } else {
            Err(errors)
        }
    }

    pub fn dependencies_for(&self, task_id: &str) -> &[String] {
        self.dependencies
            .get(task_id)
            .map(Vec::as_slice)
            .unwrap_or(&[])
    }
}

pub fn plan_dispatch(
    store: &TaskStore,
    config: &Config,
    dry_run: bool,
) -> Result<DispatchPlan, StoreError> {
    let tasks = store.load_tasks()?;
    Ok(plan_dispatch_for_tasks(&tasks, config, dry_run))
}

pub fn plan_dispatch_for_tasks(
    tasks: &[StoredTask],
    config: &Config,
    _dry_run: bool,
) -> DispatchPlan {
    let running = tasks
        .iter()
        .filter(|stored| stored.task.status == Status::Running)
        .count();
    let max_parallel = config.max_parallel as usize;
    let available_slots = max_parallel.saturating_sub(running);
    let task_map: HashMap<&str, &Task> = tasks
        .iter()
        .map(|stored| (stored.task.id.as_str(), &stored.task))
        .collect();
    let graph: HashMap<String, Vec<String>> = tasks
        .iter()
        .map(|stored| (stored.task.id.clone(), stored.task.depends_on.clone()))
        .collect();

    let mut cycle_members = HashMap::<String, Vec<String>>::new();
    for cycle in detect_cycles(&graph) {
        for id in &cycle {
            cycle_members.insert(id.clone(), cycle.clone());
        }
    }

    let mut decisions = Vec::with_capacity(tasks.len());
    for stored in tasks {
        let mut reasons = eligibility_reasons(&stored.task, config, &task_map);
        if let Some(cycle) = cycle_members.get(&stored.task.id) {
            reasons.push(SkipReason::DependencyCycle {
                cycle: cycle.clone(),
            });
        }
        decisions.push(EligibilityDecision {
            task_id: stored.task.id.clone(),
            eligible: reasons.is_empty(),
            reasons,
        });
    }

    decisions.sort_by(|left, right| {
        let left_task = task_map.get(left.task_id.as_str()).copied();
        let right_task = task_map.get(right.task_id.as_str()).copied();
        match (left_task, right_task) {
            (Some(left), Some(right)) => left
                .priority
                .cmp(&right.priority)
                .then(left.order.cmp(&right.order))
                .then(left.created_at.cmp(&right.created_at))
                .then(left.id.cmp(&right.id)),
            _ => left.task_id.cmp(&right.task_id),
        }
    });

    let mut selected = Vec::new();
    let mut remaining_slots = available_slots;
    for decision in &mut decisions {
        if !decision.reasons.is_empty() {
            decision.eligible = false;
            continue;
        }
        if remaining_slots == 0 {
            decision.eligible = false;
            decision.reasons.push(SkipReason::ConcurrencyLimit {
                max_parallel: config.max_parallel,
                running,
            });
            continue;
        }
        remaining_slots -= 1;
        decision.eligible = true;
        selected.push(decision.task_id.clone());
    }

    DispatchPlan {
        decisions,
        selected,
        running,
        available_slots,
    }
}

pub fn eligibility_reasons(
    task: &Task,
    config: &Config,
    tasks: &HashMap<&str, &Task>,
) -> Vec<SkipReason> {
    let mut reasons = Vec::new();

    if task.status != Status::Ready {
        reasons.push(SkipReason::NotReady {
            status: task.status.clone(),
        });
    }

    if !config.policy.auto_dispatch {
        reasons.push(SkipReason::AutoDispatchDisabled);
    }

    if config.policy.require_allowed_auto_dispatch && !task.allowed_auto_dispatch {
        reasons.push(SkipReason::TaskNotOptedIn);
    }

    if config.policy.require_allowed_label
        && !task.labels.iter().any(|label| {
            config
                .policy
                .allowed_labels
                .iter()
                .any(|allowed| allowed == label)
        })
    {
        reasons.push(SkipReason::MissingAllowedLabel {
            allowed: config.policy.allowed_labels.clone(),
        });
    }

    for label in &task.labels {
        if config
            .policy
            .blocked_labels
            .iter()
            .any(|blocked| blocked == label)
        {
            reasons.push(SkipReason::BlockedLabel {
                label: label.clone(),
            });
        }
    }

    if task.attempts >= task.max_attempts {
        reasons.push(SkipReason::MaxAttemptsReached {
            attempts: task.attempts,
            max_attempts: task.max_attempts,
        });
    }

    for dependency in &task.depends_on {
        match tasks.get(dependency.as_str()) {
            Some(dep_task) if dep_task.status == Status::Done => {}
            Some(dep_task) => reasons.push(SkipReason::WaitingOnDependency {
                dependency: dependency.clone(),
                status: dep_task.status.clone(),
            }),
            None => reasons.push(SkipReason::UnknownDependency {
                dependency: dependency.clone(),
            }),
        }
    }

    if task.sections.goal.is_none() {
        reasons.push(SkipReason::MissingSection { section: "Goal" });
    }
    if task.sections.context.is_none() {
        reasons.push(SkipReason::MissingSection { section: "Context" });
    }
    if task.sections.constraints.is_none() {
        reasons.push(SkipReason::MissingSection {
            section: "Constraints",
        });
    }
    if task.sections.acceptance.is_none() {
        reasons.push(SkipReason::MissingSection {
            section: "Acceptance",
        });
    } else if config.policy.require_acceptance_section && !task.sections.has_non_empty_acceptance()
    {
        reasons.push(SkipReason::EmptyAcceptance);
    }

    let agent_config = match config.agents.get(&task.agent) {
        Some(agent) => Some(agent),
        None => {
            reasons.push(SkipReason::UnknownAgent {
                agent: task.agent.clone(),
            });
            None
        }
    };

    let backend_name = agent_config
        .map(|agent| agent.backend.as_str())
        .unwrap_or(task.backend.as_str());
    if backend_name != "noop"
        && backend_name != "shell"
        && !config.backends.contains_key(backend_name)
    {
        reasons.push(SkipReason::UnknownBackend {
            backend: backend_name.to_owned(),
        });
    }

    reasons
}

pub fn skip_reason_text(reason: &SkipReason) -> String {
    match reason {
        SkipReason::NotReady { status } => format!("status is {status}, not ready"),
        SkipReason::AutoDispatchDisabled => "auto dispatch disabled by policy".to_owned(),
        SkipReason::TaskNotOptedIn => "task did not opt in to auto dispatch".to_owned(),
        SkipReason::MissingAllowedLabel { allowed } => {
            format!("missing allowed label: {}", allowed.join(", "))
        }
        SkipReason::BlockedLabel { label } => format!("blocked label: {label}"),
        SkipReason::MaxAttemptsReached {
            attempts,
            max_attempts,
        } => {
            format!("max attempts reached: {attempts}/{max_attempts}")
        }
        SkipReason::UnknownDependency { dependency } => format!("unknown dependency: {dependency}"),
        SkipReason::WaitingOnDependency { dependency, status } => {
            format!("waiting on {dependency} ({status})")
        }
        SkipReason::MissingSection { section } => format!("missing section: {section}"),
        SkipReason::EmptyAcceptance => "empty acceptance section".to_owned(),
        SkipReason::UnknownAgent { agent } => format!("unknown agent: {agent}"),
        SkipReason::UnknownBackend { backend } => format!("unknown backend: {backend}"),
        SkipReason::BackendRejected { reason } => format!("backend rejected: {reason}"),
        SkipReason::ConcurrencyLimit {
            max_parallel,
            running,
        } => {
            format!("concurrency limit reached: {running}/{max_parallel}")
        }
        SkipReason::DependencyCycle { cycle } => {
            format!("dependency cycle: {}", cycle.join(" -> "))
        }
    }
}

fn detect_cycles(graph: &HashMap<String, Vec<String>>) -> Vec<Vec<String>> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Mark {
        Visiting,
        Visited,
    }

    fn visit(
        node: &str,
        graph: &HashMap<String, Vec<String>>,
        marks: &mut HashMap<String, Mark>,
        stack: &mut Vec<String>,
        cycles: &mut Vec<Vec<String>>,
    ) {
        if marks.get(node) == Some(&Mark::Visited) {
            return;
        }
        if marks.get(node) == Some(&Mark::Visiting) {
            if let Some(pos) = stack.iter().position(|id| id == node) {
                let mut cycle = stack[pos..].to_vec();
                cycle.push(node.to_owned());
                if !cycles.iter().any(|existing| same_cycle(existing, &cycle)) {
                    cycles.push(cycle);
                }
            }
            return;
        }

        marks.insert(node.to_owned(), Mark::Visiting);
        stack.push(node.to_owned());
        if let Some(dependencies) = graph.get(node) {
            for dependency in dependencies {
                if graph.contains_key(dependency) {
                    visit(dependency, graph, marks, stack, cycles);
                }
            }
        }
        stack.pop();
        marks.insert(node.to_owned(), Mark::Visited);
    }

    let mut marks = HashMap::new();
    let mut stack = Vec::new();
    let mut cycles = Vec::new();
    let mut nodes: Vec<&String> = graph.keys().collect();
    nodes.sort();
    for node in nodes {
        visit(node, graph, &mut marks, &mut stack, &mut cycles);
    }
    cycles
}

fn same_cycle(left: &[String], right: &[String]) -> bool {
    if left.len() != right.len() {
        return false;
    }
    let Some(first) = left.first() else {
        return right.is_empty();
    };
    for offset in 0..right.len() {
        if &right[offset] == first
            && left
                .iter()
                .enumerate()
                .all(|(idx, value)| value == &right[(offset + idx) % right.len()])
        {
            return true;
        }
    }
    false
}
