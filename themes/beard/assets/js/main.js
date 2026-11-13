function initMain() {
    var indexCache = null;
    var indexPromise = null;

    function getIndex() {
        if (indexCache) return Promise.resolve(indexCache);
        if (!indexPromise) {
            indexPromise = fetch('/index.json')
                .then(function(res) { return res.ok ? res.json() : { posts: [] }; })
                .then(function(data) {
                    indexCache = (data && Array.isArray(data.posts)) ? data.posts : [];
                    return indexCache;
                })
                .catch(function() {
                    indexCache = [];
                    return indexCache;
                });
        }
        return indexPromise;
    }

    function normalizePath(pathname) {
        if (!pathname) return '/';
        return pathname.replace(/\/+$/, '') || '/';
    }

    function escapeHtml(str) {
        return String(str)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;');
    }

    function highlightText(text, terms) {
        var safe = escapeHtml(text || '');
        if (!terms || terms.length === 0) return safe;
        terms.forEach(function(term) {
            var clean = term.replace(/^#/, '');
            if (clean.length < 2) return;
            var re = new RegExp('(' + clean.replace(/[.*+?^${}()|[\]\\]/g, '\\$&') + ')', 'ig');
            safe = safe.replace(re, '<mark>$1</mark>');
        });
        return safe;
    }

    // 1. Night Ink Theme Toggle & Embedded Artifact Sync
    function currentTheme() {
        return document.documentElement.dataset.theme === 'dark' ? 'dark' : 'light';
    }

    function syncArtifactsTheme() {
        var theme = currentTheme();
        document.querySelectorAll('.chat-artifact-frame').forEach(function(frame) {
            if (frame.contentWindow) {
                try {
                    frame.contentWindow.postMessage({ type: 'ws-theme', theme: theme }, '*');
                } catch (e) {}
            }
        });
    }

    function toggleTheme() {
        var root = document.documentElement;
        var isDark = root.dataset.theme === 'dark';
        if (isDark) {
            delete root.dataset.theme;
            try { localStorage.setItem('ws-theme', 'light'); } catch (e) {}
        } else {
            root.dataset.theme = 'dark';
            try { localStorage.setItem('ws-theme', 'dark'); } catch (e) {}
        }
        syncArtifactsTheme();
    }

    window.addEventListener('message', function(e) {
        if (!e.data || typeof e.data !== 'object') return;
        if (e.data.type === 'ws-artifact-ready' && e.source) {
            try {
                e.source.postMessage({ type: 'ws-theme', theme: currentTheme() }, '*');
            } catch (err) {}
        } else if (e.data.type === 'ws-artifact-height' && typeof e.data.height === 'number') {
            document.querySelectorAll('.chat-artifact-frame').forEach(function(frame) {
                if (frame.contentWindow === e.source) {
                    frame.style.height = Math.max(180, Math.ceil(e.data.height)) + 'px';
                }
            });
        }
    });
    syncArtifactsTheme();

    var themeBtn = document.getElementById('theme-toggle');
    var mobileThemeBtn = document.getElementById('mobile-theme-toggle');
    var modalThemeBtn = document.getElementById('search-toggle-theme');
    if (themeBtn) themeBtn.addEventListener('click', toggleTheme);
    if (mobileThemeBtn) mobileThemeBtn.addEventListener('click', toggleTheme);
    if (modalThemeBtn) {
        modalThemeBtn.addEventListener('click', function() {
            toggleTheme();
        });
    }

    // 2. Cmd+K Command Search Modal
    var searchModal = document.getElementById('search-modal');
    var searchInput = document.getElementById('search-modal-input');
    var defaultPanel = document.getElementById('search-default-panel');
    var resultsPanel = document.getElementById('search-results-panel');
    var resultsCount = document.getElementById('search-results-count');
    var resultsList = document.getElementById('search-results-list');
    var selectedIdx = -1;

    function getVisibleResultLinks() {
        if (!searchModal) return [];
        var container = (resultsPanel && !resultsPanel.classList.contains('hidden')) ? resultsPanel : defaultPanel;
        if (!container) return [];
        return Array.prototype.slice.call(container.querySelectorAll('.search-result-item'));
    }

    function updateSelection(links, nextIdx) {
        links.forEach(function(el) { el.classList.remove('is-selected'); });
        if (nextIdx >= 0 && nextIdx < links.length) {
            selectedIdx = nextIdx;
            links[selectedIdx].classList.add('is-selected');
            links[selectedIdx].scrollIntoView({ block: 'nearest' });
        } else {
            selectedIdx = -1;
        }
    }

    function openSearch() {
        if (!searchModal) return;
        searchModal.classList.add('is-open');
        searchModal.setAttribute('aria-hidden', 'false');
        document.body.style.overflow = 'hidden';
        selectedIdx = -1;
        getIndex(); // warm cache
        setTimeout(function() {
            if (searchInput) {
                searchInput.focus();
                searchInput.select();
            }
        }, 20);
    }

    function closeSearch() {
        if (!searchModal) return;
        searchModal.classList.remove('is-open');
        searchModal.setAttribute('aria-hidden', 'true');
        document.body.style.overflow = '';
    }

    function scoreAndRenderSearch(query) {
        var raw = (query || '').trim();
        if (!raw) {
            if (defaultPanel) defaultPanel.classList.remove('hidden');
            if (resultsPanel) resultsPanel.classList.add('hidden');
            selectedIdx = -1;
            return;
        }

        if (defaultPanel) defaultPanel.classList.add('hidden');
        if (resultsPanel) resultsPanel.classList.remove('hidden');

        var terms = raw.toLowerCase().split(/\s+/).filter(Boolean);
        getIndex().then(function(posts) {
            var matches = [];
            posts.forEach(function(p) {
                var titleL = (p.title || '').toLowerCase();
                var subL = (p.subtitle || '').toLowerCase();
                var secL = (p.section || '').toLowerCase();
                var sumL = (p.summary || '').toLowerCase();
                var tagsL = (p.tags || []).map(function(t) { return String(t).toLowerCase(); });

                var score = 0;
                var allMatch = terms.every(function(term) {
                    if (term.charAt(0) === '#') {
                        var tq = term.slice(1);
                        var tagHit = tagsL.some(function(t) { return t.indexOf(tq) !== -1; });
                        if (tagHit) score += 12;
                        return tagHit;
                    }
                    var hit = false;
                    if (titleL.indexOf(term) !== -1) { score += 10; hit = true; }
                    if (tagsL.some(function(t) { return t.indexOf(term) !== -1; })) { score += 8; hit = true; }
                    if (subL.indexOf(term) !== -1) { score += 5; hit = true; }
                    if (secL.indexOf(term) !== -1) { score += 3; hit = true; }
                    if (sumL.indexOf(term) !== -1) { score += 2; hit = true; }
                    return hit;
                });

                if (allMatch) {
                    matches.push({ post: p, score: score });
                }
            });

            matches.sort(function(a, b) {
                if (b.score !== a.score) return b.score - a.score;
                return (b.post.date || '').localeCompare(a.post.date || '');
            });

            var top = matches.slice(0, 24);
            if (resultsCount) {
                resultsCount.textContent = matches.length === 0
                    ? 'No matching posts'
                    : (matches.length + ' ' + (matches.length === 1 ? 'result' : 'results') + (matches.length > 24 ? ' (showing top 24)' : ''));
            }

            if (resultsList) {
                resultsList.innerHTML = top.map(function(m, idx) {
                    var p = m.post;
                    var tagsHtml = (p.tags || []).slice(0, 3).map(function(t) {
                        return '<span>#' + escapeHtml(t) + '</span>';
                    }).join(' ');
                    return '<li>' +
                        '<a href="' + escapeHtml(p.url) + '" class="search-result-item' + (idx === 0 ? ' is-selected' : '') + '">' +
                        '<div class="search-result-top">' +
                        '<span class="section-badge section-badge--' + escapeHtml(p.section) + '"><span class="section-dot" aria-hidden="true"></span>' + escapeHtml(p.section) + '</span>' +
                        '<span class="search-result-meta">' + escapeHtml(p.dateShort) + ' &middot; ' + p.readingTime + 'm</span>' +
                        '</div>' +
                        '<div class="search-result-title">' + highlightText(p.title, terms) + '</div>' +
                        (p.subtitle ? '<div class="search-result-sub">' + highlightText(p.subtitle, terms) + '</div>' : '') +
                        (tagsHtml ? '<div class="search-result-meta" style="margin-top:0.2rem">' + tagsHtml + '</div>' : '') +
                        '</a>' +
                        '</li>';
                }).join('');
                selectedIdx = top.length > 0 ? 0 : -1;
            }
        });
    }

    var searchToggle = document.getElementById('search-toggle');
    var mobileSearchToggle = document.getElementById('mobile-search-toggle');
    if (searchToggle) searchToggle.addEventListener('click', openSearch);
    if (mobileSearchToggle) mobileSearchToggle.addEventListener('click', openSearch);
    document.querySelectorAll('[data-open-search]').forEach(function(btn) {
        btn.addEventListener('click', openSearch);
    });
    document.querySelectorAll('[data-search-close]').forEach(function(el) {
        el.addEventListener('click', closeSearch);
    });

    if (searchInput) {
        searchInput.addEventListener('input', function() {
            scoreAndRenderSearch(searchInput.value);
        });

        searchInput.addEventListener('keydown', function(e) {
            var links = getVisibleResultLinks();
            if (e.key === 'ArrowDown') {
                e.preventDefault();
                if (links.length) updateSelection(links, (selectedIdx + 1) % links.length);
            } else if (e.key === 'ArrowUp') {
                e.preventDefault();
                if (links.length) updateSelection(links, (selectedIdx - 1 + links.length) % links.length);
            } else if (e.key === 'Enter' && selectedIdx >= 0 && links[selectedIdx]) {
                e.preventDefault();
                window.location.href = links[selectedIdx].getAttribute('href');
            }
        });
    }

    document.addEventListener('keydown', function(e) {
        if ((e.metaKey || e.ctrlKey) && e.key.toLowerCase() === 'k') {
            e.preventDefault();
            if (searchModal && searchModal.classList.contains('is-open')) {
                closeSearch();
            } else {
                openSearch();
            }
        } else if (e.key === 'Escape' && searchModal && searchModal.classList.contains('is-open')) {
            closeSearch();
        }
    });

    // 3. Mobile Nav
    var hamburger = document.getElementById('hamburger-btn');
    var mobileNav = document.getElementById('mobile-nav');
    var mobileNavClose = document.getElementById('mobile-nav-close');
    if (hamburger && mobileNav) {
        hamburger.addEventListener('click', function() {
            mobileNav.classList.add('open');
            mobileNav.setAttribute('aria-hidden', 'false');
        });
    }
    if (mobileNavClose && mobileNav) {
        mobileNavClose.addEventListener('click', function() {
            mobileNav.classList.remove('open');
            mobileNav.setAttribute('aria-hidden', 'true');
        });
    }

    // 4. Mobile TOC Accordion + Active Section Sliding Bar + Reading Progress
    var tocToggle = document.getElementById('toc-toggle');
    var toc = document.getElementById('toc');
    if (tocToggle && toc) {
        if (window.innerWidth <= 960) {
            toc.classList.add('collapsed');
            tocToggle.setAttribute('aria-expanded', 'false');
        }
        tocToggle.addEventListener('click', function() {
            if (window.innerWidth > 960) return;
            toc.classList.toggle('collapsed');
            var expanded = !toc.classList.contains('collapsed');
            tocToggle.setAttribute('aria-expanded', expanded ? 'true' : 'false');
        });
    }

    var progressBar = document.getElementById('reading-progress');
    var proseEl = document.getElementById('article-prose');
    var tocTrack = document.getElementById('toc-track');
    var tocActiveBar = document.getElementById('toc-active-bar');
    var tocLinks = tocTrack ? Array.prototype.slice.call(tocTrack.querySelectorAll('#TableOfContents a')) : [];
    var headings = [];

    if (proseEl) {
        headings = Array.prototype.slice.call(proseEl.querySelectorAll('h2[id], h3[id]'));

        // Inject copy-permalink "#" anchors on article headings
        headings.forEach(function(h) {
            var anchor = document.createElement('a');
            anchor.className = 'heading-anchor';
            anchor.href = '#' + h.id;
            anchor.setAttribute('aria-label', 'Copy link to section');
            anchor.textContent = '#';
            anchor.addEventListener('click', function(e) {
                e.preventDefault();
                var url = window.location.origin + window.location.pathname + '#' + h.id;
                history.replaceState(null, '', '#' + h.id);
                if (navigator.clipboard && navigator.clipboard.writeText) {
                    navigator.clipboard.writeText(url).catch(function() {});
                }
                anchor.textContent = 'copied';
                anchor.classList.add('is-copied');
                setTimeout(function() {
                    anchor.textContent = '#';
                    anchor.classList.remove('is-copied');
                }, 1400);
            });
            h.appendChild(anchor);
        });
    }

    function updateReadingUI() {
        if (progressBar) {
            var doc = document.documentElement;
            var scrollTop = window.scrollY || doc.scrollTop;
            var maxScroll = (proseEl ? (proseEl.offsetTop + proseEl.offsetHeight - window.innerHeight * 0.6) : (doc.scrollHeight - window.innerHeight));
            var ratio = maxScroll > 0 ? Math.min(1, Math.max(0, scrollTop / maxScroll)) : 0;
            progressBar.style.setProperty('--reading-progress', ratio.toFixed(4));
        }

        if (tocLinks.length && headings.length) {
            var activeHeading = headings[0];
            var triggerY = window.scrollY + 120;
            for (var i = 0; i < headings.length; i++) {
                if (headings[i].offsetTop <= triggerY) {
                    activeHeading = headings[i];
                } else {
                    break;
                }
            }

            var activeLink = null;
            tocLinks.forEach(function(link) {
                var href = link.getAttribute('href') || '';
                var match = activeHeading && href === ('#' + activeHeading.id);
                link.classList.toggle('is-active', match);
                if (match) activeLink = link;
            });

            if (tocActiveBar && tocTrack && activeLink && activeLink.offsetHeight > 0) {
                var trackRect = tocTrack.getBoundingClientRect();
                var linkRect = activeLink.getBoundingClientRect();
                var topOffset = linkRect.top - trackRect.top + 3;
                tocActiveBar.style.opacity = '1';
                tocActiveBar.style.height = Math.max(14, linkRect.height - 6) + 'px';
                tocActiveBar.style.transform = 'translateY(' + topOffset + 'px)';
            }
        }
    }

    if (progressBar || tocLinks.length) {
        var ticking = false;
        window.addEventListener('scroll', function() {
            if (!ticking) {
                window.requestAnimationFrame(function() {
                    updateReadingUI();
                    ticking = false;
                });
                ticking = true;
            }
        }, { passive: true });
        window.addEventListener('resize', updateReadingUI, { passive: true });
        updateReadingUI();
    }

    // 5. Internal Link Hover Preview Popover (desktop)
    var popover = document.getElementById('link-preview-popover');
    if (popover && proseEl && window.matchMedia('(hover: hover)').matches) {
        var hoverTimer = null;
        var activeAnchor = null;
        var postMap = null;

        function getPostMap() {
            if (postMap) return Promise.resolve(postMap);
            return getIndex().then(function(posts) {
                postMap = {};
                posts.forEach(function(p) {
                    postMap[normalizePath(p.url)] = p;
                });
                return postMap;
            });
        }

        function hidePopover() {
            clearTimeout(hoverTimer);
            activeAnchor = null;
            popover.classList.remove('is-visible');
            popover.setAttribute('aria-hidden', 'true');
        }

        function positionPopover(anchor) {
            var rect = anchor.getBoundingClientRect();
            var popW = Math.min(340, window.innerWidth - 24);
            var left = Math.min(Math.max(12, rect.left), window.innerWidth - popW - 12);
            var top = rect.bottom + 10;
            if (top + 150 > window.innerHeight && rect.top > 170) {
                top = rect.top - popover.offsetHeight - 10;
            }
            popover.style.left = left + 'px';
            popover.style.top = top + 'px';
        }

        proseEl.addEventListener('mouseover', function(e) {
            var link = e.target.closest('a[href]');
            if (!link || link.classList.contains('heading-anchor')) return;
            var href = link.getAttribute('href');
            if (!href || href.charAt(0) === '#') return;
            var parsed;
            try { parsed = new URL(href, window.location.origin); } catch (err) { return; }
            if (parsed.origin !== window.location.origin && parsed.hostname !== 'willschenk.com') return;

            var key = normalizePath(parsed.pathname);
            if (key === normalizePath(window.location.pathname)) return;

            activeAnchor = link;
            clearTimeout(hoverTimer);
            hoverTimer = setTimeout(function() {
                getPostMap().then(function(map) {
                    var data = map[key];
                    if (!data || activeAnchor !== link) return;
                    popover.querySelector('.link-preview-section').textContent = data.section;
                    popover.querySelector('.link-preview-date').textContent = data.dateShort + ' · ' + data.readingTime + 'm';
                    popover.querySelector('.link-preview-title').textContent = data.title;
                    var subEl = popover.querySelector('.link-preview-sub');
                    subEl.textContent = data.subtitle || data.summary || '';
                    subEl.style.display = subEl.textContent ? 'block' : 'none';
                    var extraEl = popover.querySelector('.link-preview-extra');
                    extraEl.textContent = data.backlinks > 0
                        ? ('Referenced in ' + data.backlinks + (data.backlinks === 1 ? ' post' : ' posts'))
                        : '';
                    extraEl.style.display = extraEl.textContent ? 'block' : 'none';
                    popover.setAttribute('aria-hidden', 'false');
                    positionPopover(link);
                    popover.classList.add('is-visible');
                });
            }, 140);
        });

        proseEl.addEventListener('mouseout', function(e) {
            if (activeAnchor && !activeAnchor.contains(e.relatedTarget)) {
                hidePopover();
            }
        });
        window.addEventListener('scroll', hidePopover, { passive: true });
    }

    // 6. Dynamic View Transition Title Handoff
    var articleHeading = document.querySelector('article.single h1[data-vt-slug]');
    if (articleHeading) {
        articleHeading.style.viewTransitionName = 'post-title';
    }
    document.addEventListener('click', function(e) {
        var link = e.target.closest('a[data-vt-link]');
        if (!link) return;
        var target = link.querySelector('[data-vt-target]');
        if (target) {
            document.querySelectorAll('[data-vt-target]').forEach(function(el) {
                el.style.viewTransitionName = '';
            });
            target.style.viewTransitionName = 'post-title';
        }
    });

    // 7. Transcript Prompt Copy & Artifact Source Drawer
    document.addEventListener('click', function(e) {
        var copyBtn = e.target.closest('[data-copy-prompt]');
        if (copyBtn) {
            var fig = copyBtn.closest('.chat-prompt');
            var body = fig ? fig.querySelector('.chat-prompt-body') : null;
            var text = body ? body.innerText.trim() : '';
            if (text && navigator.clipboard && navigator.clipboard.writeText) {
                navigator.clipboard.writeText(text).catch(function() {});
            }
            copyBtn.textContent = 'copied';
            copyBtn.classList.add('is-copied');
            setTimeout(function() {
                copyBtn.textContent = 'copy';
                copyBtn.classList.remove('is-copied');
            }, 1300);
            return;
        }

        var srcBtn = e.target.closest('[data-toggle-source]');
        if (srcBtn) {
            var artifact = srcBtn.closest('.chat-artifact');
            var pre = artifact ? artifact.querySelector('.chat-artifact-source') : null;
            if (pre) {
                var hidden = pre.classList.toggle('hidden');
                srcBtn.setAttribute('aria-expanded', hidden ? 'false' : 'true');
            }
            return;
        }

        var openBtn = e.target.closest('[data-open-artifact]');
        if (openBtn) {
            var figArtifact = openBtn.closest('.chat-artifact');
            var frame = figArtifact ? figArtifact.querySelector('iframe.chat-artifact-frame') : null;
            var html = frame ? frame.getAttribute('srcdoc') : '';
            if (html) {
                var blobUrl = URL.createObjectURL(new Blob([html], { type: 'text/html' }));
                window.open(blobUrl, '_blank', 'noopener');
            }
        }
    });
}

if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initMain);
} else {
    initMain();
}
