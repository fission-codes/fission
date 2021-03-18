{ pkgs, unstable, server-path, server-port, ... }:
  let
    bash    = "${pkgs.bash}/bin/bash";
    git     = "${pkgs.git}/bin/git";
    killall = "${pkgs.killall}/bin/killall";
    ssh     = "${pkgs.openssh}/bin/ssh";
    stack   = "${unstable.stack}/bin/stack";
    figlet  = "${pkgs.figlet}/bin/figlet";
    lolcat  = "${pkgs.lolcat}/bin/lolcat";

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

    # This will be much better when we have a nix-build
    server-install = cmd "Install the Fission Server"
      "${stack} install --nix fission-web-server:fission-server";

    server-start = cmd "Run the currently installed Fission Server"
      "DEBUG=true nohup ${server-path} &";

    server-debug = cmd "Run the Fission Server in debug verbose mode"
      "DEBUG=true ${server-path}";

  in
    commands {
      welcome = cmd "Print the pretty welcome" ''
        echo "🌈✨ Welcome to the glorious... "
        ${figlet} "Fission Build Env" | ${lolcat} -a -s 50
      '';

      build       = cmd "Build entire project"    "${stack} build   --nix";
      cli-install = cmd "Install the Fission CLI" "${stack} install --nix fission-cli:fission";

      inherit
        server-install
        server-start
        server-debug;

      server-update = cmd "Update & run the current server to the latest on the current branch" ''
        ${git} pull \
        && ${server-install.script} \
        && ${killall} fission-server \
        ;  printf "🚨 Don't forget to release a new version of the CLI 📟✨" \
        && ${server-start.script}
      '';

      quality = cmd "Run the complete test suite" "${stack} test  --nix --test-arguments='--color=always'";
      repl    = cmd "Enter the project REPL"      "${stack} repl  --nix --no-nix-pure";
      watch   = cmd "Autobuild with file watcher" "${stack} build --nix --file-watch";

      ssh-staging = cmd "SSH into the staging environment"
        "${ssh} fission@instance.runfission.net";

      ssh-prod = cmd "SSH into the production environment"
        "${ssh} ubuntu@instance.runfission.com";
    }
