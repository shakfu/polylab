import { invoke } from "@tauri-apps/api/core";
import { ask, message, open, save } from "@tauri-apps/plugin-dialog";
import { BaseDirectory, exists, mkdir, readTextFile, remove, writeTextFile } from "@tauri-apps/plugin-fs";
import { revealItemInDir } from "@tauri-apps/plugin-opener";

import { log } from "../lib/console";
import { $, errText, setOutput } from "../lib/dom";

interface FilePreview {
  path: string;
  name: string;
  sizeBytes: number;
  isUtf8: boolean;
  lineCount: number;
  preview: string;
}

const SCRATCH_FILE = "scratchpad.txt";

export function initFilesPanel(): void {
  initDialogs();
  initScratchpad();
}

function initDialogs(): void {
  const out = $("#file-out");
  const reveal = $<HTMLButtonElement>("#reveal-file");
  let lastPath: string | null = null;

  $("#pick-file").addEventListener("click", async () => {
    // `open` only returns a path -- it never reads the file itself.
    const picked = await open({
      multiple: false,
      directory: false,
      title: "Pick any file to preview",
    });
    if (!picked) {
      setOutput(out, "dialog cancelled");
      return;
    }

    try {
      const preview = await invoke<FilePreview>("read_file_preview", {
        path: picked,
        maxBytes: 2048,
      });
      lastPath = preview.path;
      reveal.disabled = false;
      setOutput(
        out,
        [
          `${preview.name}  (${formatBytes(preview.sizeBytes)}, ${
            preview.isUtf8 ? `${preview.lineCount} lines of text` : "binary"
          })`,
          preview.path,
          "",
          preview.preview.slice(0, 1200),
        ].join("\n"),
        "ok",
      );
      log("dialog/open", preview.name, "ok");
    } catch (error) {
      setOutput(out, errText(error), "err");
      log("dialog/open", errText(error), "err");
    }
  });

  $("#save-file").addEventListener("click", async () => {
    const path = await save({
      title: "Save a sample file",
      defaultPath: "tauri-demo.txt",
      filters: [{ name: "Text", extensions: ["txt", "md"] }],
    });
    if (!path) {
      setOutput(out, "save cancelled");
      return;
    }
    try {
      const written = await invoke<number>("write_text_file", {
        path,
        contents: `Written by the Tauri demo at ${new Date().toISOString()}\n`,
      });
      lastPath = path;
      reveal.disabled = false;
      setOutput(out, `wrote ${written} bytes to\n${path}`, "ok");
      log("dialog/save", `${written} bytes`, "ok");
    } catch (error) {
      setOutput(out, errText(error), "err");
    }
  });

  $("#ask-dialog").addEventListener("click", async () => {
    const answer = await ask("Native dialogs block the window they belong to. Continue?", {
      title: "A question",
      kind: "warning",
    });
    await message(answer ? "You chose yes." : "You chose no.", { title: "Result" });
    log("dialog/ask", `answer: ${answer}`);
  });

  reveal.addEventListener("click", async () => {
    if (!lastPath) return;
    await revealItemInDir(lastPath);
    log("opener/reveal", lastPath);
  });
}

function initScratchpad(): void {
  const textarea = $<HTMLTextAreaElement>("#scratch");
  const status = $("#scratch-status");

  const report = (text: string) => {
    status.textContent = text;
  };

  const loadScratch = async () => {
    try {
      if (await exists(SCRATCH_FILE, { baseDir: BaseDirectory.AppData })) {
        textarea.value = await readTextFile(SCRATCH_FILE, { baseDir: BaseDirectory.AppData });
        report(`loaded $APPDATA/${SCRATCH_FILE}`);
      } else {
        report(`no $APPDATA/${SCRATCH_FILE} yet`);
      }
    } catch (error) {
      report(errText(error));
    }
  };

  $("#scratch-save").addEventListener("click", async () => {
    try {
      // The app data directory does not necessarily exist on first run.
      await mkdir("", { baseDir: BaseDirectory.AppData, recursive: true });
      await writeTextFile(SCRATCH_FILE, textarea.value, { baseDir: BaseDirectory.AppData });
      report(`saved ${textarea.value.length} chars to $APPDATA/${SCRATCH_FILE}`);
      log("fs/write", SCRATCH_FILE, "ok");
    } catch (error) {
      report(errText(error));
      log("fs/write", errText(error), "err");
    }
  });

  $("#scratch-load").addEventListener("click", () => void loadScratch());

  $("#scratch-clear").addEventListener("click", async () => {
    try {
      if (await exists(SCRATCH_FILE, { baseDir: BaseDirectory.AppData })) {
        await remove(SCRATCH_FILE, { baseDir: BaseDirectory.AppData });
      }
      textarea.value = "";
      report("file deleted");
      log("fs/remove", SCRATCH_FILE, "warn");
    } catch (error) {
      report(errText(error));
    }
  });

  void loadScratch();
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KiB`;
  return `${(bytes / 1024 / 1024).toFixed(1)} MiB`;
}
