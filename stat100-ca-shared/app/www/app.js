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
