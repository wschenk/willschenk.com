import { supabase } from "./db.js";
import { notify } from "./notify.js";

class ProfilePanel extends HTMLElement {
  constructor() {
    super();
    this.state = "anonymous";
  }

  connectedCallback() {
    const { localdata } = supabase.auth.onAuthStateChange((event, session) => {
      notify(event);
      if (event == "SIGNED_IN") {
        this.state = "authenticated";
        this.session = session;
        console.log("session", session);
      } else if (event == "SIGNED_OUT") {
        this.state = "anonymous";
        this.session = null;
      }
      this.render();
    });

    this.data = localdata;

    this.render();
  }

  disconnectedCallback() {
    this.data.subscription.unsubscribe();
  }

    render() {
        if( this.state != 'authenticated' ) {
            this.innerHTML = '<anonymous-profile>Anonymous</anonymous-profile>';
        } else {
            this.innerHTML = `<authed-profile email="${this.session.user.email}">Logged in</authed-profile>`
        }
    }
}

customElements.define("profile-panel", ProfilePanel);
