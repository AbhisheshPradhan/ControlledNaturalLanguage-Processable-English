(function($) {
    $(document).ready(function() {
        $(".dropdown-menu").delegate("li", "click", function() {
            var str = JSON.stringify($(this).html());
            var addRegex = /Add/i
            var saveRegex = /Save/i
            var loadRegex = /Load/i

            var notFile = !(addRegex.test(str) || saveRegex.test(str));

            if (notFile) {
                $(this).addClass("active").siblings().removeClass("active");
            }

        });
        // FOR MENU COLOUR
        $('ul.dropdown-menu [data-toggle=dropdown]').on('click', function(event) {
            event.preventDefault();
            event.stopPropagation();
            $(this).parent().siblings().removeClass('open');
            $(this).parent().toggleClass('open');
        });
        // NEED FOR TABS
        $('.collapse').on('shown.bs.collapse', function() {
            $(this).parent().find(".glyphicon-plus-sign").removeClass("glyphicon-plus-sign").addClass("glyphicon-minus-sign");
        }).on('hidden.bs.collapse', function() {
            $(this).parent().find(".glyphicon-minus-sign").removeClass("glyphicon-minus-sign").addClass("glyphicon-plus-sign");
        });
        /// for asp toggle (dev mode)
        $("[name='my-checkbox']").bootstrapSwitch();
        $('input[name="my-checkbox"]').on('switchChange.bootstrapSwitch', function(event, state) {
            if (state) {
                viewModel.asp(viewModel.bigAsp);
            } else {
                viewModel.asp(viewModel.smallAsp);
            }
            viewModel.aspState = state;
        });
    });

    $(".results-button").click(function() {
        console.log("clicked")
        icon = $(this).find("i");
        if (icon.hasClass("fa-minus")) {
            icon.addClass("fa-plus").removeClass("fa-minus");
        } else {
            icon.addClass("fa-minus").removeClass("fa-plus");
        }
    });

})(jQuery);