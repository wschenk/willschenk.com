import {supabase} from './db.js';
import {notify} from './notify.js'

class AnonymousProfile extends HTMLElement {
    constructor() {
        super();
        this.error = ""
    }

    connectedCallback() {
        this.render();
    }

    render() {
        const errorMessage = this.error != "" ? `<p>${this.error}</p>` : '';

        this.innerHTML = `<sl-button variant="primary" id="sign-in">Sign In</sl-button>
<sl-dialog label="Signin" class="dialog-overview">
${errorMessage}
    <form>
      <sl-input id="email" name="email" label="Email" required></sl-input>
      <sl-input id="password" name="password" label="Password" type="password" required></sl-input>
      <sl-button variant="primary" id="signin">Signin</sl-button>
      <sl-button  variant="primary" id="login">Login</sl-button>
    </form>

</sl-dialog>
`;

    this.querySelector("#sign-in").addEventListener("click", () => {
      this.querySelector("sl-dialog").show();
      this.querySelector("#signin").addEventListener("click", (event) => {
        this.handleSignin(event);
      });
      this.querySelector("#login").addEventListener("click", (event) => {
        this.handleLogin(event);
      });
    });
  }

  async handleSignin(event) {
      this.error = "";
      event.preventDefault();
      const formData = new FormData(this.querySelector("form"));
      
      const email = formData.get("email");
      const password = formData.get("password");
      const { data, error } = await supabase.auth.signUp({
          email,
          password,
      });
      
      if (error) {
          this.error = error.message;
          notify(error.message, "danger");
          this.render();
          this.querySelector("sl-dialog").show();

      }
  }
    
    async handleLogin(event) {
        this.error = "";
        event.preventDefault();
        const formData = new FormData(this.querySelector("form"));
        const email = formData.get("email");
        const password = formData.get("password");

        const { data, error } = await supabase.auth.signInWithPassword({
            email,
            password,
        });
        
        if (error) {
            this.error = error.message;
            notify(error.message, "danger");
            this.render();
            this.querySelector("sl-dialog").show();

        }
    }

}

customElements.define("anonymous-profile", AnonymousProfile );
