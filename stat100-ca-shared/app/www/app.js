// Copy buttons: <button class="copy-btn" data-target="<id of textarea/input>">
document.addEventListener('click', function (e) {
  var b = e.target.closest('.copy-btn');
  if (!b) return;
  var el = document.getElementById(b.dataset.target);
  if (!el) return;
  navigator.clipboard.writeText(el.value).then(function () {
    var old = b.textContent; b.textContent = 'Copied!';
    setTimeout(function () { b.textContent = old; }, 1500);
  });
});
// Keep the "Draft in my email app" link in sync with edits to subject/body.
document.addEventListener('input', function (e) {
  var panel = e.target.closest('.email-panel');
  if (!panel) return;
  var q = function (s) { return panel.querySelector(s); };
  var to = q('textarea[readonly]').value, subj = q('input[type=text]').value, body = q('textarea:not([readonly])').value;
  q('a.btn-primary').href = 'mailto:?bcc=' + encodeURIComponent(to.replace(/\s+/g, '')) +
    '&subject=' + encodeURIComponent(subj) + '&body=' + encodeURIComponent(body);
});
// Menus in the top bar (e.g. "View as"): keep the whole menu on screen.
// When the bar wraps onto two lines, "View as" can sit at the left edge;
// a right-aligned menu would then open off the left side of the window.
document.addEventListener('shown.bs.dropdown', function (e) {
  var menu = e.target.parentElement && e.target.parentElement.querySelector('.dropdown-menu');
  if (!menu || !menu.closest('.navbar')) return;
  menu.style.left = ''; menu.style.right = '';
  var r = menu.getBoundingClientRect();
  if (r.left < 8) { menu.style.left = '0'; menu.style.right = 'auto'; }
  r = menu.getBoundingClientRect();
  if (r.right > window.innerWidth - 8) { menu.style.left = 'auto'; menu.style.right = '0'; }
});
