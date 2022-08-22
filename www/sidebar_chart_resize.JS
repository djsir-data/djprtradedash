$(document).ready(function () {
  $(".sidebar-toggle").click(function() {
    for(var i = Highcharts.charts.length - 1; i >= 0; --i){
      chart = Highcharts.charts[i];
      chart.reflow();
    }
  });
});
