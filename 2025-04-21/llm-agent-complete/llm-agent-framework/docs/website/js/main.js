// Main JavaScript for GoAgent website
document.addEventListener('DOMContentLoaded', function() {
    // Mobile navigation toggle
    const navToggle = document.createElement('button');
    navToggle.classList.add('nav-toggle');
    navToggle.innerHTML = '<span></span><span></span><span></span>';
    
    const nav = document.querySelector('nav');
    if (window.innerWidth <= 768) {
        nav.style.display = 'none';
        document.querySelector('header .container').appendChild(navToggle);
        
        navToggle.addEventListener('click', function() {
            if (nav.style.display === 'none') {
                nav.style.display = 'block';
                navToggle.classList.add('active');
            } else {
                nav.style.display = 'none';
                navToggle.classList.remove('active');
            }
        });
    }
    
    // Smooth scrolling for anchor links
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function (e) {
            e.preventDefault();
            
            const target = document.querySelector(this.getAttribute('href'));
            if (target) {
                target.scrollIntoView({
                    behavior: 'smooth'
                });
            }
        });
    });
    
    // Code copy button
    document.querySelectorAll('pre code').forEach(block => {
        const copyButton = document.createElement('button');
        copyButton.className = 'copy-button';
        copyButton.textContent = 'Copy';
        
        const pre = block.parentNode;
        pre.style.position = 'relative';
        pre.appendChild(copyButton);
        
        copyButton.addEventListener('click', () => {
            navigator.clipboard.writeText(block.textContent).then(() => {
                copyButton.textContent = 'Copied!';
                setTimeout(() => {
                    copyButton.textContent = 'Copy';
                }, 2000);
            });
        });
    });
    
    // Trace viewer functionality (for example pages)
    const traceViewers = document.querySelectorAll('.trace-viewer');
    traceViewers.forEach(viewer => {
        const traceData = viewer.getAttribute('data-trace');
        if (traceData) {
            try {
                const events = JSON.parse(traceData);
                renderTraceEvents(viewer, events);
            } catch (e) {
                console.error('Error parsing trace data:', e);
            }
        }
    });
    
    function renderTraceEvents(container, events) {
        events.forEach(event => {
            const eventEl = document.createElement('div');
            eventEl.className = 'trace-event';
            
            const typeEl = document.createElement('div');
            typeEl.className = 'trace-event-type';
            typeEl.textContent = event.type || 'Event';
            
            const dataEl = document.createElement('div');
            dataEl.className = 'trace-event-data';
            dataEl.textContent = JSON.stringify(event.data || event, null, 2);
            
            const timestampEl = document.createElement('div');
            timestampEl.className = 'trace-event-timestamp';
            timestampEl.textContent = new Date(event.timestamp || Date.now()).toISOString();
            
            eventEl.appendChild(typeEl);
            eventEl.appendChild(dataEl);
            eventEl.appendChild(timestampEl);
            
            container.appendChild(eventEl);
        });
    }
    
    // Highlight active navigation item based on current page
    const currentPath = window.location.pathname;
    document.querySelectorAll('nav a').forEach(link => {
        if (link.getAttribute('href') === currentPath || 
            (currentPath.includes(link.getAttribute('href')) && link.getAttribute('href') !== '/')) {
            link.classList.add('active');
        }
    });
    
    // Documentation sidebar highlight active section
    const docSidebar = document.querySelector('.doc-sidebar');
    if (docSidebar) {
        const headings = document.querySelectorAll('.doc-content h2, .doc-content h3');
        const sidebarLinks = document.querySelectorAll('.doc-sidebar a');
        
        if (headings.length > 0 && sidebarLinks.length > 0) {
            window.addEventListener('scroll', () => {
                let currentSection = '';
                
                headings.forEach(heading => {
                    const sectionTop = heading.offsetTop;
                    if (window.scrollY >= sectionTop - 100) {
                        currentSection = heading.getAttribute('id');
                    }
                });
                
                sidebarLinks.forEach(link => {
                    link.classList.remove('active');
                    if (link.getAttribute('href').includes(currentSection)) {
                        link.classList.add('active');
                    }
                });
            });
        }
    }
});
