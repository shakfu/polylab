import { getCurrentWindow } from "@tauri-apps/api/window";
import { WebviewWindow } from "@tauri-apps/api/webviewWindow";

import { log } from "../lib/console";
import { $, errText, setOutput } from "../lib/dom";

const EXTRA_LABEL = "inspector";

export function initWindowPanel(): void {
  initControls();
  initExtraWindow();
}

function initControls(): void {
  // `getCurrentWindow` is a handle to the window this webview lives in, so the
  // same code works unchanged in a second window.
  const win = getCurrentWindow();
  const onTop = $<HTMLButtonElement>("#win-ontop");
  let pinned = false;

  $("#win-minimize").addEventListener("click", () => void win.minimize());
  $("#win-maximize").addEventListener("click", () => void win.toggleMaximize());

  $("#win-fullscreen").addEventListener("click", async () => {
    await win.setFullscreen(!(await win.isFullscreen()));
  });

  onTop.addEventListener("click", async () => {
    pinned = !pinned;
    await win.setAlwaysOnTop(pinned);
    onTop.textContent = `Always on top: ${pinned ? "on" : "off"}`;
    log("window/setAlwaysOnTop", String(pinned));
  });

  $("#win-center").addEventListener("click", () => void win.center());

  const titleInput = $<HTMLInputElement>("#title-input");
  $("#title-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    const title = titleInput.value.trim() || "Tauri feature demo";
    await win.setTitle(title);
    log("window/setTitle", title, "ok");
  });
}

function initExtraWindow(): void {
  const openButton = $<HTMLButtonElement>("#win-new");
  const closeButton = $<HTMLButtonElement>("#win-close-extra");
  const out = $("#win-out");

  const reset = () => {
    closeButton.disabled = true;
    openButton.disabled = false;
    setOutput(out, "no extra window");
  };

  openButton.addEventListener("click", () => {
    // Creating a window is asynchronous and reports back through events; the
    // constructor itself returns immediately.
    const extra = new WebviewWindow(EXTRA_LABEL, {
      url: "index.html",
      title: "Inspector (second webview)",
      width: 620,
      height: 520,
    });

    void extra.once("tauri://created", () => {
      openButton.disabled = true;
      closeButton.disabled = false;
      setOutput(
        out,
        [
          `label     ${EXTRA_LABEL}`,
          "url       index.html (a full second instance of this UI)",
          "note      app://tick reaches both windows",
        ].join("\n"),
        "ok",
      );
      log("webview/create", EXTRA_LABEL, "ok");
    });

    void extra.once<{ message?: string }>("tauri://error", (event) => {
      setOutput(out, errText(event.payload?.message ?? event.payload), "err");
      reset();
    });

    void extra.once("tauri://destroyed", () => {
      log("webview/destroyed", EXTRA_LABEL, "warn");
      reset();
    });
  });

  closeButton.addEventListener("click", async () => {
    const extra = await WebviewWindow.getByLabel(EXTRA_LABEL);
    await extra?.close();
  });
}

/** Reflects the Rust-side close-to-tray setting in the Windows panel. */
export function showCloseBehaviour(enabled: boolean): void {
  setOutput(
    $("#close-out"),
    enabled
      ? "close to tray: on -- the close button hides the window, quit from the tray menu"
      : "close to tray: off -- the close button quits the app",
  );
}
