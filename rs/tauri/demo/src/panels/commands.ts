import { invoke } from "@tauri-apps/api/core";

import { log } from "../lib/console";
import { $, errText, pretty, setOutput } from "../lib/dom";

interface Greeting {
  message: string;
  greetedAtMs: number;
  from: string;
}

interface Note {
  id: number;
  title: string;
  body: string;
  done: boolean;
}

interface NoteStats {
  total: number;
  done: number;
  open: number;
}

export function initCommandsPanel(): void {
  initGreet();
  initDivide();
  initNotes();
  void refreshNotes();
}

// --- greet ---------------------------------------------------------------

function initGreet(): void {
  const input = $<HTMLInputElement>("#greet-input");
  const out = $("#greet-out");

  $("#greet-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    try {
      // Arguments are camelCase here and snake_case in Rust; Tauri converts.
      const greeting = await invoke<Greeting>("greet", { name: input.value });
      setOutput(out, pretty(greeting), "ok");
      log("invoke/greet", greeting.message, "ok");
    } catch (error) {
      setOutput(out, errText(error), "err");
      log("invoke/greet", errText(error), "err");
    }
  });
}

// --- deliberate failure --------------------------------------------------

function initDivide(): void {
  const a = $<HTMLInputElement>("#divide-a");
  const b = $<HTMLInputElement>("#divide-b");
  const out = $("#divide-out");

  $("#divide-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    try {
      const result = await invoke<number>("divide", {
        numerator: Number(a.value),
        denominator: Number(b.value),
      });
      setOutput(out, `= ${result}`, "ok");
    } catch (error) {
      setOutput(out, `rejected: ${errText(error)}`, "err");
      log("invoke/divide", errText(error), "warn");
    }
  });
}

// --- notes (managed state) ----------------------------------------------

function initNotes(): void {
  const title = $<HTMLInputElement>("#note-title");
  const body = $<HTMLInputElement>("#note-body");
  const error = $("#note-error");

  $("#note-form").addEventListener("submit", async (event) => {
    event.preventDefault();
    error.hidden = true;
    try {
      const note = await invoke<Note>("add_note", {
        title: title.value,
        body: body.value,
      });
      title.value = "";
      body.value = "";
      log("state/add_note", `#${note.id} ${note.title}`, "ok");
      await refreshNotes();
    } catch (err) {
      error.textContent = errText(err);
      error.hidden = false;
    }
    title.focus();
  });

  // One listener for the whole list rather than one per row.
  $("#note-list").addEventListener("click", async (event) => {
    const button = (event.target as HTMLElement).closest<HTMLButtonElement>("button[data-action]");
    if (!button) return;

    const id = Number(button.dataset.id);
    const command = button.dataset.action === "toggle" ? "toggle_note" : "delete_note";
    try {
      await invoke<Note>(command, { id });
      log(`state/${command}`, `#${id}`, "ok");
      await refreshNotes();
    } catch (err) {
      log(`state/${command}`, errText(err), "err");
    }
  });
}

export async function refreshNotes(): Promise<void> {
  const list = $<HTMLUListElement>("#note-list");
  const stats = $("#note-stats");

  const [notes, counts] = await Promise.all([
    invoke<Note[]>("list_notes"),
    invoke<NoteStats>("note_stats"),
  ]);

  list.innerHTML = "";
  for (const note of notes) {
    list.append(renderNote(note));
  }
  stats.textContent = notes.length
    ? `${counts.total} note(s) in Rust memory -- ${counts.open} open, ${counts.done} done`
    : "no notes yet -- add one, or use Actions > New note in the menu bar";
}

function renderNote(note: Note): HTMLLIElement {
  const li = document.createElement("li");
  li.className = note.done ? "note is-done" : "note";

  const id = document.createElement("span");
  id.className = "note-id";
  id.textContent = `#${note.id}`;

  const text = document.createElement("span");
  text.className = "note-text";
  const strong = document.createElement("b");
  strong.textContent = note.title;
  text.append(strong);
  if (note.body) {
    const body = document.createElement("span");
    body.className = "note-body";
    body.textContent = note.body;
    text.append(body);
  }

  const toggle = document.createElement("button");
  toggle.className = "ghost tiny";
  toggle.dataset.action = "toggle";
  toggle.dataset.id = String(note.id);
  toggle.textContent = note.done ? "Reopen" : "Done";

  const remove = document.createElement("button");
  remove.className = "ghost tiny";
  remove.dataset.action = "delete";
  remove.dataset.id = String(note.id);
  remove.textContent = "Delete";

  li.append(id, text, toggle, remove);
  return li;
}

/** Called by the native menu / tray "New note" item. */
export function focusNoteForm(): void {
  $<HTMLInputElement>("#note-title").focus();
}
