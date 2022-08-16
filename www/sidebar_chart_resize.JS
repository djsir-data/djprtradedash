$(document).ready(function () {
  $(document).on('change.sidebarmenuExpandedInputBinding', function() {
    for(var i = Highcharts.charts.length - 1; i >= 0; --i){
      chart = Highcharts.charts[i];
      chart.reflow();
    }
  });
});
