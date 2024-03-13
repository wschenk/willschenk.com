class AvatarImage extends HTMLElement {
    connectedCallback() {
        let image = this.querySelector('img')

        image.style.cssText = `
    width: 8rem;
    height: 8rem;
    box-shadow: 0 20px 25px -5px rgba(0,0,0,.1), 0 10px 10px -5px rgba(0,0,0,.04);
    border-width: 2px;
    border-radius: 9999px;
    --border-opacity: 1;
    border-color: #edf2f7;
    border-color: rgba(237, 242, 247, var(--border-opacity));
    max-width: 100%;
    display: block;
    vertical-align: middle;
    border-style: solid;
`;

        this.insertAdjacentHTML('beforeend', `
<div style="width: auto;
  display: none;
  max-width: 20%;
  height: auto;
  min-height: 25px;
  line-height: 25px;
  font-size: 1rem;
  background-color: rgba(0, 0, 0, 0.7);
  color: #ffffff;
  border-radius: 5px;
  margin-top: 10px;
  padding: 10px 15px;">${image.getAttribute('alt')}</div>
`)

        let div = this.querySelector("div")

        image.addEventListener( "mouseenter", () => div.style.display = 'block' )
        image.addEventListener( "mouseout", () => div.style.display = 'none' )

    }
}

customElements.define( 'avatar-image', AvatarImage )
