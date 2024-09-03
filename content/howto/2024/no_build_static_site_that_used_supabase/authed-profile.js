import { supabase } from "./db.js";
import { notify } from "./notify.js";

class AuthedProfile extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        this.innerHTML = `
<sl-dropdown class="profile-dropdown">
  <sl-avatar slot="trigger"></sl-avatar>
  <sl-menu>
    <sl-menu-item>${this.getAttribute("email")}</sl-menu-item>
    <sl-divider></sl-divider>
    <sl-menu-item id="logout">Logout</sl-menu-item>
  </sl-menu>
</sl-dropdown>
`;

        const menu = document.querySelector(".profile-dropdown");

        menu.addEventListener("sl-select", (event) => {
            console.log("sl-select", event.detail.item);
            if (event.detail.item.id === "logout") {
                supabase.auth.signOut();
            }
        });
    }
}

customElements.define("authed-profile", AuthedProfile);
