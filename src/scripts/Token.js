var token = {
    postToken(word) {
        var wordData = textLineData.createNode(word, viewModel.reasonerMode);
        var request = $.ajax({
            url: "/peng",
            type: "POST",
            data: wordData,
            async: false
        });
        return request;
    }
}
