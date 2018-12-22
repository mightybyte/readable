{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc"
, rpSha ? "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j"
, system ? builtins.currentSystem
}:

let rp = builtins.fetchTarball {
      url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
      sha256 = rpSha;
    };

in
  (import rp {}).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "ee230196f4ce4778bfe0e84429966a343fcc5248";
        sha256 = "10md0iaic858fvwvc71qs50w13240fwq5rvza9kd2la3s94mzawy";
      }) {};
  in
  {
    name = "readable";
    overrides = self: super: with pkgs.haskell.lib; {};
    packages = {
      # Temporarily disable gitignoreSource due to https://github.com/siers/nix-gitignore/issues/14
      readable = gitignore.gitignoreSource [] ./.;
      # readable = builtins.filterSource
      #   (path: type: !(builtins.elem (baseNameOf path) ["result" "dist" "dist-newstyle" ".git" ".stack-work"]))
      #   ./.;
    };
    shells = {
      ghc = ["readable"];
    };

  })
