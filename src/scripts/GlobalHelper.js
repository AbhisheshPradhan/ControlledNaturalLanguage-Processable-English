// Helper functions to reduce repeated code

let globalHelper = {
  createJsonObject: function (emode, token, fname, stext, reason, rmode) {
    let object = {
      id: -1,
      inputmode: "text",
      editmode: emode,
      token: token,
      featurestructure: "{ \"cat\" : \" \",  \"wform\" : \" \"}",
      filename: fname,
      spectext: stext,
      snum: -1,
      spos: -1,
      reasoner: reason,
      reasonermode: rmode
    }
    return object;
  }
}