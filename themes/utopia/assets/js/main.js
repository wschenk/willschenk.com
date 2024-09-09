document.addEventListener('DOMContentLoaded', (event) => {
    new PagefindUI({
        element: "#search",
        showImages: false
    });

    const element = document.querySelector("#search");
    const headerLinks = document.querySelectorAll("header.body a")
    const trigger = headerLinks[headerLinks.length-1];

    trigger.addEventListener('click', () => {
        element.classList.toggle('hidden');
        element.querySelector("input").value=""
        element.querySelector("input").focus()
    });

    document.addEventListener('keydown', function(event) {
        if (event.metaKey && event.key === 'k') {
            // Code to execute when Command + K is pressed
            console.log('Command + K pressed!');

            // Prevent default behavior (e.g., opening a browser search bar)
            event.preventDefault();

            element.classList.toggle('hidden');
            element.querySelector("input").value=""
            element.querySelector("input").focus()
        }
    });
});
