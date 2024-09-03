// db.js
import { createClient } from "https://cdn.jsdelivr.net/npm/@supabase/supabase-js/+esm";

let api_url="https://aqghyiuxzwrxqfmcnpmo.supabase.co"
let anon_key="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZSIsInJlZiI6ImFxZ2h5aXV4endyeHFmbWNucG1vIiwicm9sZSI6ImFub24iLCJpYXQiOjE3MjUyNDU1MDMsImV4cCI6MjA0MDgyMTUwM30.oqSJJOh6fErPZxutNJgBHH_Y4eGo3XNgppgOsu2zz4g"

if ( window.location.hostname ==  '127.0.0.1' ) {
    api_url="http://127.0.0.1:54321";
    anon_key="eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJpc3MiOiJzdXBhYmFzZS1kZW1vIiwicm9sZSI6ImFub24iLCJleHAiOjE5ODM4MTI5OTZ9.CRXP1A7WOeoJeXxjNni43kdQwgnWNReilDMblYTn_I0"
}

export const supabase = createClient(api_url, anon_key);

// db.js continued
export async function getPosts() {
  const { data, error } = await supabase
    .from("posts")
    .select("*")
    .order("created_at", { ascending: true });
    
    if (error) {
        console.log( error );
    }

    return {data, error};
}

export async function createPost(title, body) {
    const { data, error } = await supabase.from("posts").insert({ title, body });

    if (error) {
        console.log( error );
    }

    return {data, error};
}
