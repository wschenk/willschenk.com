// notify.js
// Always escape HTML for text arguments!
export function escapeHtml(html) {
    const div = document.createElement("div");
    div.textContent = html;
    return div.innerHTML;
}

// Custom function to emit toast notifications
export async function notify(
    message,
    variant = "primary",
    icon = "info-circle",
    duration = 3000
) {
    const alert = Object.assign(document.createElement("sl-alert"), {
        variant,
        closable: true,
        duration: duration,
        innerHTML: `
        <sl-icon name="${icon}" slot="icon"></sl-icon>
        ${escapeHtml(message)}
      `,
    });
    document.body.append(alert);
    setTimeout(() => {
        alert.toast();
    }, 250);
}
