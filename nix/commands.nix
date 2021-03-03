{ pkgs, unstable, ... }:
  let
    bash  = "${pkgs.bash}/bin/bash";
    ssh   = "${pkgs.openssh}/bin/ssh";
    stack = "${unstable.stack}/bin/stack";
    ghcid = "${pkgs.ghcid}/bin/ghcid";

    cmd = description: script:
      { inherit description;
        inherit script;
      };

    command = {name, script, description ? "<No description given>"}:
      let
        package =
          pkgs.writeScriptBin name ''
            #!${bash}
            echo "⚙️  Running ${name}..."
            unset STACK_IN_NIX_SHELL
            ${script}
          '';

        bin = "${package}/bin/${name}";
      in
         { package     = package;
           description = description;
           bin         = bin;
         };

    commands = defs:
      let
        names =
          builtins.attrNames defs;

        helper =
          let
            lengths = map builtins.stringLength names;
            maxLen  = builtins.foldl' (acc: x: if x > acc then x else acc) 0 lengths;
            maxPad  =
              let
                go = acc:
                  if builtins.stringLength acc >= maxLen
                  then acc
                  else go (" " + acc);
              in
                go "";

            folder = acc: name:
              let
                nameLen = builtins.stringLength name;
                padLen  = maxLen - nameLen;
                padding = builtins.substring 0 padLen maxPad;
              in
                acc + " && echo '${name} ${padding}| ${(builtins.getAttr name defs).description}'";

            lines =
              builtins.foldl' folder "echo ''" names;

          in
            pkgs.writeScriptBin "helpme" ''
              #!${pkgs.stdenv.shell}
              ${pkgs.figlet}/bin/figlet "Commands" | ${pkgs.lolcat}/bin/lolcat
              ${toString lines}
            '';

        mapper = name:
          let
            element =
              builtins.getAttr name defs;

            task = command {
              inherit name;
              description = element.description;
              script      = element.script;
            };
          in
            task.package;

        packages =
          map mapper names;

      in
        [helper] ++ packages;

  in
    commands {
      build          = cmd "Build entire project"       "${stack} build";
      install-cli    = cmd "Install the Fission CLI"    "${stack} install fission-cli:fission";
      install-server = cmd "Install the Fission Server" "${stack} install fission-web-server:server";

      runtests = cmd "Run the complete test suite" "${stack} test";
      repl     = cmd "Enter the project REPL"      "${stack} repl  --no-nix-pure";
      watch    = cmd "Autobuild with file watcher" "${stack} build --file-watch";

      ssh-staging = cmd "SSH into the staging environment"
        "${ssh} ubuntu@instance.runfission.net";

      ssh-prod = cmd "SSH into the production environment"
        "${ssh} ubuntu@instance.runfission.com";
    }
