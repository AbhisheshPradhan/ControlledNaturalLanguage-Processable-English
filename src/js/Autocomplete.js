$(function() {
    function split(val) {
        return val.split(/,\s*/);
    }

    function extractLast(term) {
        var last = split(term).pop();
        return last;
    }
    $("#text_field")
        .on("keydown", function(event) {
            if (event.keyCode === $.ui.keyCode.TAB &&
                $(this).autocomplete("instance").menu.active) {
                event.preventDefault();
            }
        })
        .autocomplete({
            minLength: 0,
            source: function(request, response) {
                response($.ui.autocomplete.filter(generateList(), extractLast(viewModel.textAreaStr())));
            },
            open: function() {
                $("ul.ui-menu").width($(this).innerWidth() - 19);
            },
            focus: function() {
                return false;
            },
            select: function(event, ui) {
                var terms = split(this.value);
                viewModel.textAreaStr(ui.item.value);
                terms.pop();
                terms.push(" ");
                this.value = terms.join(" ");
                var temp = ui.item.value.split(" ");
                viewModel.textAreaStr()
                viewModel.token(temp.pop());
                this.value = this.value.slice(0, this.value.length - 2);
                return false;
            }
        });
    generateList = function() {
        if (viewModel.lookUpTable().length < 11 && viewModel.allowInput) {
            var s = viewModel.textAreaStr().split(" ");
            s.pop();
            s = s.join(" ");
            var result = [];
            for (var i = 0; i < viewModel.lookUpTable().length; i++) {
                result.push(s + " " + viewModel.lookUpTable()[i]);
            }
            return result;
        }
        return [];
    }
});