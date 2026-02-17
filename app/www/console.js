// Scenario Console Output — real-time pipeline message display
// =============================================================================

$(document).ready(function() {

  Shiny.addCustomMessageHandler('scenario_console_log', function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    el.textContent += msg.text;
    el.scrollTop = el.scrollHeight;
  });

  Shiny.addCustomMessageHandler('scenario_console_clear', function(msg) {
    var el = document.getElementById(msg.id);
    if (!el) return;
    el.textContent = '';
  });

});
