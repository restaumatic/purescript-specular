{ pkgs ? import <nixpkgs> {}, ... }:
with pkgs;
pkgs.mkShell {
  buildInputs = with pkgs; [
    chromium
  ];
  PUPPETEER_SKIP_CHROMIUM_DOWNLOAD = true;
  PUPPETEER_EXECUTABLE_PATH= "${pkgs.chromium.outPath}/bin/chromium";
}
