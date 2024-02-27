document.addEventListener("DOMContentLoaded", () => {
    for (let item of document.querySelectorAll( "[remote-html]" )) {
        fetch( item.attributes['remote-html'].value )
            .then( (response) => {return response.text()} )
            .then( (html) => {
                item.innerHTML = html
                document
                    .querySelectorAll( `a[href='${window.location.pathname}']`)
                    .forEach((el) => {
                        el.classList.add('font-bold');
                    });
            } )

    }
});

document.addEventListener("DOMContentLoaded", () => {
    for (let item of document.querySelectorAll( "[remote-json]" )) {
        fetch( item.attributes['remote-json'].value )
            .then( (response) => {return response.json()} )
            .then( (json) => {
                item.innerHTML = JSON.stringify(json, null, 2)
            } )

    }
});

document.addEventListener("DOMContentLoaded", () => {
    for (let item of document.querySelectorAll( "[remote-template]" )) {
        const template = item.getElementsByTagName("template")[0].getInnerHTML()

        const handler = new Function( 'i', 'const tagged = (i) => `' + template + '`; return tagged(i)')

        fetch( item.attributes['remote-template'].value )
            .then( (response) => {return response.json()} )
            .then( (json) => {
                for( let i of json ) {
                    item.innerHTML += handler(i);
                }})
    }
});
