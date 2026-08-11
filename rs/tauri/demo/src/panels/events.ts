import { Channel, invoke } from "@tauri-apps/api/core";
import { emit, listen } from "@tauri-apps/api/event";

import { log } from "../lib/console";
import { $, errText, setOutput } from "../lib/dom";

export interface Tick {
  seq: number;
  uptimeSecs: number;
  openNotes: number;
}

/**
 * The payload of the channel used by `run_job`. Serde's `tag = "kind"` on the
 * Rust enum turns it into a discriminated union on this side.
 */
type JobEvent =
  | { kind: "started"; steps: number }
  | { kind: "progress"; step: number; steps: number; percent: number; note: string }
  | { kind: "finished"; steps: number; checksum: string }
  | { kind: "cancelled"; atStep: number };

export async function initEventsPanel(): Promise<void> {
  await initTicker();
  initUiLog();
  initJob();
}

async function initTicker(): Promise<void> {
  const out = $("#tick-out");
  // `listen` resolves to an unlisten function; keep it if the subscription is
  // ever scoped to a view rather than the whole app.
  await listen<Tick>("app://tick", (event) => {
    const { seq, uptimeSecs, openNotes } = event.payload;
    setOutput(
      out,
      [
        `event    ${event.event}`,
        `seq      ${seq}`,
        `uptime   ${uptimeSecs}s`,
        `open     ${openNotes} note(s)`,
      ].join("\n"),
    );
  });
}

function initUiLog(): void {
  const input = $<HTMLInputElement>("#uilog-input");
  $("#uilog-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    const message = input.value.trim() || "hello from the webview";
    await emit("ui://log", { message, at: new Date().toISOString() });
    log("emit/ui://log", `${message} (check the tauri dev terminal)`, "ok");
    input.value = "";
  });
}

function initJob(): void {
  const form = $("#job-form");
  const start = $<HTMLButtonElement>("#job-start");
  const cancel = $<HTMLButtonElement>("#job-cancel");
  const steps = $<HTMLInputElement>("#job-steps");
  const ms = $<HTMLInputElement>("#job-ms");
  const bar = $("#job-bar");
  const out = $("#job-out");

  cancel.addEventListener("click", () => {
    void invoke("cancel_job");
    log("invoke/cancel_job", "cancellation requested", "warn");
  });

  form.addEventListener("submit", async (event) => {
    event.preventDefault();
    start.disabled = true;
    cancel.disabled = false;
    bar.style.width = "0%";

    const channel = new Channel<JobEvent>();
    const lines: string[] = [];
    channel.onmessage = (message) => {
      switch (message.kind) {
        case "started":
          lines.length = 0;
          lines.push(`started: ${message.steps} steps`);
          break;
        case "progress":
          bar.style.width = `${message.percent}%`;
          lines.push(`  ${String(message.step).padStart(3)}/${message.steps}  ${message.note}`);
          break;
        case "finished":
          bar.style.width = "100%";
          lines.push(`finished: checksum ${message.checksum}`);
          break;
        case "cancelled":
          lines.push(`cancelled at step ${message.atStep}`);
          break;
      }
      // Keep the tail visible without unbounded growth.
      setOutput(out, lines.slice(-12).join("\n"), message.kind === "cancelled" ? "err" : "plain");
    };

    try {
      const result = await invoke<string>("run_job", {
        steps: Number(steps.value),
        stepMs: Number(ms.value),
        onEvent: channel,
      });
      log("channel/run_job", result, "ok");
    } catch (error) {
      setOutput(out, errText(error), "err");
      log("channel/run_job", errText(error), "err");
    } finally {
      start.disabled = false;
      cancel.disabled = true;
    }
  });
}
