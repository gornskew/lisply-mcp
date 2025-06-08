# Projects Directory

## Common Lisp Service

There is a Common Lisp service running on localhost that handles eval requests.

### Testing the Service

To test the Lisp evaluation service:

```bash
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}'
```

Expected response format:
```json
{"success":true,"result":"6","stdout":""}
```

The service expects JSON with a `code` field containing the Lisp expression to evaluate.

**Note:** Do not include the `Content-Type: application/json` header - the service works without it and including it may cause issues with some tools.

**Important for Claude Code:** When using the Bash tool to call the service, the JSON response will be treated as an "error" by default. To properly capture the response, use output redirection:

```bash
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(+ 1 2 3)"}' 2>&1 | cat
```

This captures both stdout and stderr and pipes through cat to display the actual JSON response.

## Modern Site Development Workflow

### Loading the Development Environment

1. **Load Quicklisp:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(load-quicklisp)"}'
   ```

2. **Add local project directory:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(pushnew \"~/projects/apps/\" ql:*local-project-directories* :test #'\''equalp)"}'
   ```

3. **Load the website systems:**
   ```bash
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :modern-site)"}'
   curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(ql:quickload :royalties)"}'
   ```

### Website Structure

The modern-site application runs at http://localhost:9081/ and includes:

**Main Page Components:**
- **Assembly class** (`apps/modern-site/source/assembly.lisp:4`) - Main landing page
- **Base-site-sheet** (`apps/modern-site/source/base-site-sheet.lisp:4`) - Common layout with Bootstrap navbar, footer, and styling

**Working Internal Links:**
- `/contact/index.html` - Contact information and links
- `/about/index.html` - Licensing and pricing details  
- `/products/index.html` - Product descriptions with collapsible sections
- `/gwl/index.html` - Generative Web Language information
- `/smlib/index.html` - Solid modeling capabilities

All pages have "Home" navigation back to `/` landing page.

**Note on Initialization:**
The `(modern-site:initialize!)` function sets up web routes and static file serving, which can conflict with the HTTP eval service. Run initialization from SLIME REPL instead of through the HTTP service.

### Testing Live Pages

Use curl to test any page:
```bash
curl -s http://localhost:9081/contact/index.html | head -20
```

**Verified Working Endpoints:**
- All main pages load with proper HTML structure
- Bootstrap CSS and JavaScript assets serve correctly
- Image assets serve correctly (logos, photos)
- Navigation between pages works properly
- Static file serving is operational after initialization

**Development Notes:**
- When testing static assets with `curl -I` (HEAD requests), responses may appear as errors even when assets are serving correctly
- Use `curl -s` for content requests to properly test page functionality
- Static assets include: Bootstrap CSS/JS, custom CSS, jQuery, and image files in `/site-images/` directory

### Development Workflow with Update! Links

Both sites include **Update!** and **SetSelf** development links when `gwl:*developing?*` is enabled:

```bash
curl -X POST http://127.0.0.1:9081/lisply/lisp-eval -d '{"code": "(setq gwl:*developing?* t)"}' 2>&1 | cat
```

**Using the Development Links:**
- **Update!** - Recompiles changed source code and refreshes the page to show updates
- **SetSelf** - Sets the REPL toplevel `*self*` to the current page object for interactive development

**Iterative Development Process:**
1. Make changes to source files (`.lisp`, `.gendl`) 
2. Click **Update!** link on the web page OR use `(the update!)` if self is set via lisp-eval
3. Refresh page to see changes immediately
4. Use **SetSelf** to enable REPL commands like `(the update!)` for faster iteration

This enables rapid prototyping without restarting the entire development environment.