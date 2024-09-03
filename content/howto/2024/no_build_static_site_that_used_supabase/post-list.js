import { supabase, getPosts } from "./db.js";

class PostList extends HTMLElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
    this.posts = [];
  }

  async connectedCallback() {
    this.render();
      const {data,error} = await getPosts();
      this.posts = data;
      if( error ) {
          notify( error.message, "danger" );
          }
    this.channel = supabase
      .channel("schema-db-changes")
      .on(
        "postgres_changes",
        {
          event: "*",
          schema: "public",
        },
        (payload) => {
          console.log(payload);
          if (payload.eventType === "INSERT") {
            this.posts.push(payload.new);
            this.render();
          }
        }
      )
      .subscribe();
    this.render();
  }

  disconnectedCallback() {
    this.channel.unsubscribe();
  }

  render() {
    this.shadowRoot.innerHTML = `
      <style>
        ul {
          list-style: none;
          padding: 0;
        }
        h2 {
          font-size: var(--sl-font-size-large);
        }
        p {
          font-size: var(--sl-font-size-medium);
        }
          time {
          font-size: var(--sl-font-size-small);
        }
      </style>
      <ul class="posts">
      </ul>
    `;

    const ul = this.shadowRoot.querySelector("ul");
    this.posts.forEach((post) => {
      const li = document.createElement("li");
      li.innerHTML = `
<h2>${post.title}</h2>
<time>${post.created_at}</time>
</time>
<p>${post.body}</p>
`;
      ul.appendChild(li);
    });
  }
}

customElements.define("post-list", PostList);
