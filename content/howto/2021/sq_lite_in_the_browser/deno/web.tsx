import { DB } from "./deps.ts";
const ctx: Worker = self as any;

ctx.onmessage = (e) => {
    console.log( "Got message" );
    ctx.postMessage( "Hi there" );
    for (const [name] of db.query("SELECT name FROM people")) {
        console.log(name);
    }

}

const db = new DB();

db.query(
    "CREATE TABLE IF NOT EXISTS people (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT)",
);

const names = ["Peter Parker", "Clark Kent", "Bruce Wayne"];

// Run a simple query
for (const name of names) {
    db.query("INSERT INTO people (name) VALUES (?)", [name]);
}

console.log( "Loaded the worker" );
