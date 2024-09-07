{
  description = "ami-ui";

  inputs = {
    nixpkgs     = { url = "nixpkgs/nixpkgs-unstable"; };
    utils       = { url = "github:numtide/flake-utils"; };
  };
  
  outputs = { self, nixpkgs, utils }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        pname = "ami-ui";
        version = "0.1.0";
      in {
        packages = rec {
          aip-ui = pkgs.stdenv.mkDerivation rec {
            name = "${pname}";
            src = self;
            version = "${version}";
            buildPhase = ":";
            installPhase = ''
            '';
          };
          default = "ami-ui";
        };

        apps = rec {
          ami-ui = utils.lib.mkApp { drv = self.packages.${system}.ami-ui; };
          default = ami-ui;
        };

        defaultPackage = self.packages.${system}.default;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            awscli2
            cacert
            esbuild
            gnumake
            jq
            nixpkgs-fmt
            nodejs_22
            purescript
            spago
          ];

          shellHook = ''
            export LANG=en_US.UTF-8
            # awscli2 and openai have a dependency conflict 
            alias aws='PYTHONPATH= aws'
            export PS1="ami-ui|$PS1"
          '';
        };
        devShell = self.devShells.${system}.default;        
      }
    );
}
