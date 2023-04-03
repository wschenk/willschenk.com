let lastKnownScrollPosition = 0;
let shown = false;

function doSomething(scrollPos) {
  // Do something with the scroll position
}

document.addEventListener('scroll', (e) => {
  let down = lastKnownScrollPosition > window.scrollY;

  if( down && shown ) {
    window.requestAnimationFrame(() => {
      document.querySelector("header").classList = 'hidden';
      shown = false
    })
  }

  if( !down && !shown ) {
    window.requestAnimationFrame(() => {
      document.querySelector("header").classList = 'visible';
      shown = true
    })
  }
});