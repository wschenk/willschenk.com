// Expandable Code Blocks JavaScript
document.addEventListener('DOMContentLoaded', function() {
  // Find all code blocks that should be expandable (both .chroma and other types)
  const codeBlocks = document.querySelectorAll('.chroma, pre.example, pre.src, pre');

  codeBlocks.forEach(function(codeBlock) {
    // Skip if already processed
    if (codeBlock.classList.contains('expandable-processed') ||
        codeBlock.closest('.expandable-code')) {
      return;
    }

    const codeHeight = codeBlock.scrollHeight;
    const maxHeight = 400;
    const needsExpand = codeHeight > maxHeight;

    // Create wrapper for ALL code blocks (for copy button + overflow)
    const wrapper = document.createElement('div');
    wrapper.className = 'expandable-code';
    if (!needsExpand) {
      wrapper.classList.add('expanded'); // short blocks start expanded
    }
    codeBlock.parentNode.insertBefore(wrapper, codeBlock);
    wrapper.appendChild(codeBlock);

    // Only add toggle button if block is tall enough
    if (needsExpand) {
      const toggleBtn = document.createElement('button');
      toggleBtn.className = 'code-toggle';
      toggleBtn.setAttribute('type', 'button');
      toggleBtn.setAttribute('aria-label', 'Toggle code block expansion');
      wrapper.appendChild(toggleBtn);

      toggleBtn.addEventListener('click', function() {
        wrapper.classList.toggle('expanded');
        const isExpanded = wrapper.classList.contains('expanded');
        toggleBtn.setAttribute('aria-label',
          isExpanded ? 'Collapse code block' : 'Expand code block'
        );
        const codeId = generateCodeId(codeBlock);
        localStorage.setItem(`code-expanded-${codeId}`, isExpanded);
      });

      // Check for saved preference
      const codeId = generateCodeId(codeBlock);
      const wasExpanded = localStorage.getItem(`code-expanded-${codeId}`) === 'true';
      if (wasExpanded) {
        wrapper.classList.add('expanded');
        toggleBtn.setAttribute('aria-label', 'Collapse code block');
      }
    }

    // Always add copy button
    const copyBtn = document.createElement('button');
    copyBtn.className = 'code-copy';
    copyBtn.setAttribute('type', 'button');
    copyBtn.setAttribute('aria-label', 'Copy code to clipboard');
    wrapper.appendChild(copyBtn);

    copyBtn.addEventListener('click', function() {
      copyCodeToClipboard(codeBlock, copyBtn);
    });

    // Mark as processed
    codeBlock.classList.add('expandable-processed');
  });
});

// Generate a unique ID for the code block
function generateCodeId(codeBlock) {
  // Try to use existing ID or generate one
  if (codeBlock.id) {
    return codeBlock.id;
  }

  // Generate ID based on content hash
  const content = codeBlock.textContent;
  let hash = 0;
  for (let i = 0; i < content.length; i++) {
    const char = content.charCodeAt(i);
    hash = ((hash << 5) - hash) + char;
    hash = hash & hash; // Convert to 32-bit integer
  }
  return `code-${Math.abs(hash)}`;
}

// Copy code to clipboard
async function copyCodeToClipboard(codeBlock, copyBtn) {
  try {
    // Get the text content (without line numbers)
    const codeText = getCodeText(codeBlock);

    // Use modern clipboard API
    if (navigator.clipboard && window.isSecureContext) {
      await navigator.clipboard.writeText(codeText);
    } else {
      // Fallback for older browsers
      const textArea = document.createElement('textarea');
      textArea.value = codeText;
      textArea.style.position = 'fixed';
      textArea.style.left = '-999999px';
      textArea.style.top = '-999999px';
      document.body.appendChild(textArea);
      textArea.focus();
      textArea.select();
      document.execCommand('copy');
      document.body.removeChild(textArea);
    }

    // Show success feedback
    copyBtn.classList.add('copied');
    setTimeout(() => {
      copyBtn.classList.remove('copied');
    }, 2000);

  } catch (err) {
    console.error('Failed to copy code:', err);

    // Show error feedback
    copyBtn.classList.add('error');
    copyBtn.setAttribute('title', 'Failed to copy');
    setTimeout(() => {
      copyBtn.classList.remove('error');
      copyBtn.removeAttribute('title');
    }, 2000);
  }
}

// Extract code text without line numbers
function getCodeText(codeBlock) {
  // If there are line numbers, we need to extract just the code
  const lines = codeBlock.querySelectorAll('.line');
  if (lines.length > 0) {
    return Array.from(lines)
      .map(line => {
        // Remove line number and get just the code content
        const lineNumber = line.querySelector('.lnt');
        if (lineNumber) {
          lineNumber.remove();
        }
        return line.textContent || '';
      })
      .join('\n')
      .trim();
  }

  // Fallback to direct text content
  return codeBlock.textContent.trim();
}

// Keyboard navigation support
document.addEventListener('keydown', function(e) {
  // Only handle when focus is on code buttons
  if (!e.target.classList.contains('code-toggle') &&
      !e.target.classList.contains('code-copy')) {
    return;
  }

  switch (e.key) {
    case 'Enter':
    case ' ':
      e.preventDefault();
      e.target.click();
      break;
  }
});
