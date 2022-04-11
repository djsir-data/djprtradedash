

axe
  .run()
  .then(results => {
    if (results.violations.length) {

      console.log(results.violations);

      var data = results.violations;
      var issueList = '';

      function axelist(data) {
        for (var i = 0; i < data.length; i++) {
          var item = data[i];

          //console.log(item)

          var description = item.description.toLowerCase();
          var help = item.help
          //item.description = item.description.replace(new RegExp('<','g'), '&lt;');

          issueList += '<h3><a href="' + item.helpUrl + '">' + help + '</a></h3><br>' + description + '<br>'

          var node = item.nodes[0]

          issueList += '<b>'+ node.impact + '</b>' + '<br>' + node.failureSummary + '<br>'
          issueList += '<b>HTML Causing Problem</b><br>' + node.html + '<br>' + node.target.join()

          //console.log(node)

          var issues = node.any;

          if (issues.length > 0) {

            issueList += '<ul id="issue">';

            for (var ii = 0; ii < issues.length; ii++) {

              var prob = issues[ii];

              console.log('issue', prob);

              issueColor = 'red';
              issueError = prob.message;
              issueList += '<li id="issueList">' + '<span style="color:' + issueColor +'">' + prob.impact + '</span>: ' + issueError + ' </li>';



          };

          issueList += '</ul>';

        };

      };

      console.log(issueList)

      $('#axe-results').html(issueList);

      };

      axelist(data);

    }
  })
