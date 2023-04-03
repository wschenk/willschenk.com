document.querySelectorAll( 'table[x-url]' ).forEach( async (elem) => {
    const url = elem.getAttribute( 'x-url' );
    elem.innerHTML = `<p>Loading ${url}...</p>`
    
    const response = await fetch( url );
    if( !response.ok ) {
        elem.innerHTML = `<p>Error loading ${url}: ${response.statusText}</p>`;
    } else {
        const text = await response.text();
        const table = Papa.parse( text ).data;
        let html = `<caption>${url}</caption>`
        const header = table.shift();
        html += '<tr>';
        header.forEach( (key) => {html += `<th>${key}</th>`} );
        table.forEach( (row) => {
            html += '</tr><tr>';
            row.forEach( (col) => {html += `<td>${col}</td>`} );
        } );
        html += '</tr>';
        
        elem.innerHTML=html;
    }
} );

document.querySelectorAll( 'svg.line-graph[x-url]' ).forEach( async (elem) => {
    const url = elem.getAttribute( 'x-url' );
    console.log( `line-graph loading ${url}` );
    
    const response = await fetch( url );
    if( !response.ok ) {
        console.log( "Error loading ${url}: ${response.statusText}" );
    } else {
        const text = await response.text();
        const table = Papa.parse( text ).data;
        const header = table.shift();

        const data = {
            title: url,
            xLabel: 'Month',
            yLabel: 'Commits',
            data: {
                labels: [],
                datasets: [
                    { label: 'Commits', data: [] },
                    { label: 'Authors', data: [] }
                ]},
            options: {}
        };

        count = 0

        table.forEach( (row) => {
            if( row.length >= 2 ) {
                data.data.labels.push( count += 1 ); //row[0] );
                data.data.datasets[0].data.push( row[1] );
                data.data.datasets[1].data.push( row[2] );
            }
        } );

        console.log( data );
            
        new chartXkcd.Line(elem, data );
    }
} );
