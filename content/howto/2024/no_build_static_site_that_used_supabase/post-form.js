import { createPost } from "./db.js";

class PostForm extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.render();
    const form = this.querySelector("form");
    form.addEventListener("submit", this.handleSubmit.bind(this));
  }

  render() {
    this.innerHTML = `
<form class="input-validation-required">
  <sl-input name="title" label="Title" required></sl-input>
  <br />
  <sl-textarea name="body" label="Body" required></sl-textarea>
  <br /><br />
  <sl-button type="submit" variant="primary">Submit</sl-button>
</form>`;
  }

  async handleSubmit(event) {
    event.preventDefault();
    const formData = new FormData(event.target);
    const title = formData.get("title");
    const body = formData.get("body");
    await createPost(title, body);
  }
}

customElements.define("post-form", PostForm);
