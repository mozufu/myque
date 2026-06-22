{
  description = "Development environment for MyQue";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { nixpkgs, ... }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "aarch64-linux"
        "x86_64-linux"
      ];

      forAllSystems = f:
        nixpkgs.lib.genAttrs systems (system:
          f (import nixpkgs { inherit system; }));
    in
    {
      devShells = forAllSystems (pkgs:
        let
          inherit (pkgs) lib stdenv;
          darwinSdk = pkgs.apple-sdk_15;
          darwinSdkRoot = "${darwinSdk}/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk";
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              cargo
              clippy
              rustc
              rustfmt
            ] ++ lib.optionals stdenv.isDarwin [
              darwinSdk
            ];

            env = lib.optionalAttrs stdenv.isDarwin {
              LIBRARY_PATH = "${darwinSdkRoot}/usr/lib";
              SDKROOT = darwinSdkRoot;
            };
          };
        });
    };
}
