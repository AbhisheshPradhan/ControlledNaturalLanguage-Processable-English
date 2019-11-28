(function ($) {
  $(document).ready(function () {
    // Background Theory Switch
    $("[name='my-checkbox']").bootstrapSwitch();
    $('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function (event, state) {
      if (state) {
        viewModel.asp(viewModel.bigAsp);
      } else {
        viewModel.asp(viewModel.smallAsp);
      }
      viewModel.aspState = state;
    });
  });

  // Results Accordions
  $(".results-button").each(function () {
    expanded = $(this).hasClass("expanded");
    if (expanded) {
      $(this).find(".results-icon").addClass("results-icon-minus");
      $(this).find(".results-icon").removeClass("results-icon-plus");
    } else {
      $(this).find(".results-icon").addClass("results-icon-plus");
      $(this).find(".results-icon").removeClass("results-icon-minus");
    }
  });
  $(".results-button").click(function (event) {
    console.log("clicked");
    expanded = $(this).hasClass("expanded");
    if (expanded) {
      $(this).removeClass("expanded");
      $(this).find(".results-icon").addClass("results-icon-plus");
      $(this).find(".results-icon").removeClass("results-icon-minus");
    } else {
      $(this).addClass("expanded");
      $(this).find(".results-icon").addClass("results-icon-minus");
      $(this).find(".results-icon").removeClass("results-icon-plus");
    }
  });

  // Prevent default behavior of the text area html element
  $("#text_field").keypress(function (e) {
    if (e.keyCode == 13 && !e.shiftKey) {
      e.preventDefault();
      return false;
    }
  });
  $("#text_field").click(function (e) {
    console.log("text field clicked");
    e.preventDefault();
  });
  $("#text_field").keyup(function (e) {
    if (e.keyCode == 8) {
      e.preventDefault();
      return false;
    }
  });

  // initialise plugin
  // buttons to demonstrate Superfish's public methods
  $('.destroy').on('click', function () {
    example.superfish('destroy');
  });
})(jQuery);
