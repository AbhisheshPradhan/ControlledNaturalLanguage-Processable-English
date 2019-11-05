(function ($) { //create closure so we can safely use $ as alias for jQuery
  $(document).ready(function () {
    // initialise plugin
    // buttons to demonstrate Superfish's public methods
    $('.destroy').on('click', function () {
      example.superfish('destroy');
    });
  });
})(jQuery);
