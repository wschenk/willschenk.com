document.addEventListener('DOMContentLoaded', function() {
    // Pagefind search
    new PagefindUI({
        element: "#search",
        showImages: false
    });

    var searchEl = document.getElementById('search');

    // Desktop search toggle
    var searchToggle = document.getElementById('search-toggle');
    if (searchToggle) {
        searchToggle.addEventListener('click', function() {
            searchEl.classList.toggle('hidden');
            if (!searchEl.classList.contains('hidden')) {
                var input = searchEl.querySelector('input');
                if (input) {
                    input.value = '';
                    input.focus();
                }
            }
        });
    }

    // Cmd+K shortcut
    document.addEventListener('keydown', function(event) {
        if (event.metaKey && event.key === 'k') {
            event.preventDefault();
            searchEl.classList.toggle('hidden');
            if (!searchEl.classList.contains('hidden')) {
                var input = searchEl.querySelector('input');
                if (input) {
                    input.value = '';
                    input.focus();
                }
            }
        }
        // Escape to close search
        if (event.key === 'Escape' && !searchEl.classList.contains('hidden')) {
            searchEl.classList.add('hidden');
        }
    });

    // Mobile nav
    var hamburger = document.getElementById('hamburger-btn');
    var mobileNav = document.getElementById('mobile-nav');
    var mobileNavClose = document.getElementById('mobile-nav-close');

    if (hamburger && mobileNav) {
        hamburger.addEventListener('click', function() {
            mobileNav.classList.add('open');
        });
    }

    if (mobileNavClose && mobileNav) {
        mobileNavClose.addEventListener('click', function() {
            mobileNav.classList.remove('open');
        });
    }

    // TOC accordion on mobile
    var tocToggle = document.getElementById('toc-toggle');
    var toc = document.getElementById('toc');
    if (tocToggle && toc && window.innerWidth <= 900) {
        toc.classList.add('collapsed');
        tocToggle.addEventListener('click', function() {
            toc.classList.toggle('collapsed');
        });
    }
});
