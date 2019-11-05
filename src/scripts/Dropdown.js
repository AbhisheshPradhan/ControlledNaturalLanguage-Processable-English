(function ($) {
  $(document).ready(function () {
    $(".dropdown-menu").delegate("li", "click", function () {
      let str = JSON.stringify($(this).html());
      let addRegex = /Add/i
      let saveRegex = /Save/i
      let loadRegex = /Load/i

      let notFile = !(addRegex.test(str) || saveRegex.test(str));

      if (notFile) {
        $(this).addClass("active").siblings().removeClass("active");
      }

    });

    // FOR MENU COLOUR
    $('ul.dropdown-menu [data-toggle=dropdown]').on('click', function (event) {
      event.preventDefault();
      event.stopPropagation();
      $(this).parent().siblings().removeClass('open');
      $(this).parent().toggleClass('open');
    });

    // NEED FOR TABS
    $('.collapse').on('shown.bs.collapse', function () {
      $(this).parent().find(".glyphicon-plus-sign").removeClass("glyphicon-plus-sign").addClass("glyphicon-minus-sign");
    }).on('hidden.bs.collapse', function () {
      $(this).parent().find(".glyphicon-minus-sign").removeClass("glyphicon-minus-sign").addClass("glyphicon-plus-sign");
    });

    /// for asp toggle (dev mode)
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

  (function ($) {
    $(document).ready(function () {
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
        console.log("clicked")

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
    });
  })(jQuery);
})(jQuery);