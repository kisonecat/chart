let
  myNixPkgs = import <nixpkgs> {
    overlays = [myNixPkgsOverlay];
  };

  myNixPkgsOverlay = (nixSelf: nixSuper: {
    myHaskellPackages = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {})) myHaskellPkgsOverlay;
    });
  });

  
  myHaskellPkgsOverlay = (self: super: {
    doenetchart = self.callCabal2nix "doenetchart" ./. {};

    lti13 = let f = myNixPkgs.fetchFromGitHub {
        owner  = "kisonecat";
        repo   = "lti13";
        rev    =
          "7edb86b41904f57fdd7152afb55c16f87deb00d7";
          sha256 = "sha256-4+0QHmXwupEIdHQR7LAnC86qV1Avpjt9YLuzlO22IeQ=";
    }; in myNixPkgs.haskell.lib.doJailbreak (self.callCabal2nix "lti13" "${f}/lti13" {});
  });
  
  myDevTools = with myNixPkgs; [
    cabal-install 
    haskellPackages.ghcid
    haskell-language-server
    hlint
  ];

  myShellHook = ''
    alias repl="cabal new-repl"
  '';
in
myNixPkgs.myHaskellPackages.doenetchart.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ myDevTools;
  shellHook = myShellHook;
})
