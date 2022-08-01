console.log("updateNumYAxes loaded");

Shiny.addCustomMessageHandler('updateNumYAxes', function(msg) {

  var chart = $("#" + msg.id).highcharts();
  const curentLength = chart.yAxis.length;
  const targetLength = msg.nAxes;

  console.log("updateNumYAxes run");

  if(curentLength > targetLength){
    for(let i = curentLength - 1; i >= targetLength; i--){
      chart.yAxis[i].remove()
    };
  };

  if(curentLength < targetLength){
    for(let i = curentLength; i < targetLength; i++){
      chart.addaxis()
    };
  };

});
