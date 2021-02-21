{ pkgs, unstable, ... }:
  let
    ssh   = "${pkgs.openssh}/bin/ssh";
    stack = "${unstable.stack}/bin/stack";
    ghcid = "${pkgs.ghcid}/bin/ghcid";

    task = name: body:
      let
        package =
          pkgs.writeScriptBin name ''
            #!${pkgs.stdenv.shell}
            echo "⚙️  Running ${name}..."
            unset STACK_IN_NIX_SHELL
            ${body}
          '';

        bin = "${package}/bin/${name}";
      in
         { package = package;
           bin     = bin;
         };

    build    = task "build" "${stack} build";
    runtests = task "runtests" "${stack} test";
    repl     = task "repl" "${stack} repl --no-nix-pure";

    watch       = task "watch" "${stack} build --file-watch";
    watch-ghcid = task "watch-ghcid" ''
      ${ghcid} -c "unset STACK_IN_NIX_SHELL && ${stack} repl"
    '';

    ssh-staging = task "ssh-staging" "${ssh} ubuntu@instance.runfission.net";
    ssh-prod    = task "ssh-prod" "${ssh} ubuntu@instance.runfission.com";

  in
    map (e: e.package) [
      build
      repl
      ssh-prod
      ssh-staging
      runtests
      watch
      watch-ghcid
    ]
