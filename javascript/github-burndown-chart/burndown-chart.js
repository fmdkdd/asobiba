document.addEventListener('DOMContentLoaded', init)

function init() {
  var width = 900
  var height = 500

  var svg = d3.select("body").append("svg")
      .attr("width", width)
      .attr("height", height)

  d3.json("spacemacs-issues.json", function getData(err, data) {
    if (err) {
      console.error(err)
      return
    }

    var [points, total] = prepare_data(data)
    var earliest = points[0].time

    var scaleTime = d3.scaleTime()
        .domain([earliest, Date.now()])
      .range([2, width - 2])    // 2, -2 to prevent clipping for border points

    var scaleY = d3.scaleLinear()
      .domain([0, total])
      .range([height - 2, 2])   // reversed because SVG origin is top-left

    // Draw the total (open+closed) area
    var totalArea = svg.selectAll("circle.total")
      .data(points)

    totalArea.enter()
      .append("circle")
      .attr("class", "total")
      .attr("cx", d => scaleTime(d.time))
      .attr("cy", d => scaleY(d.total))
      .attr("r", 1)
      .attr("fill", "red")

    // Draw the closed area
    var closedArea = svg.selectAll("circle.closed")
      .data(points)

    closedArea.enter()
      .append("circle")
      .attr("class", "closed")
      .attr("cx", d => scaleTime(d.time))
      .attr("cy", d => scaleY(d.closed))
      .attr("r", 1)
      .attr("fill", "green")
  })
}

function prepare_data(issues) {
  // First convert all issues to events.  The point is to organize all these
  // events along time, to be able to count the running total afterwards.
  var events = []

  issues.forEach(function issueToEvents(i) {
    // Issue an event for each new issue
    events.push({
      type: "new issue",
      time: new Date(i.created_at),
    })

    // and for each closed issue
    if (i.closed_at) {
      events.push({
        type: "closed issue",
        time: new Date(i.closed_at),
      })
    }
  })

  // Now sort the events by time.  Implicitly, new issues are already sorted
  // chronologically, but closed issues are not.
  events.sort(function sortByTime(a, b) {
    return a.time - b.time
  })


  // Then count the running total of issues vs. running total of closed issues
  var points = []
  var total = 0
  var closed = 0

  // Emit a point for each event.  Points will be chronologically sorted
  events.forEach(function eventsToPoints(e) {
    if (e.type === "new issue") {
      points.push({
        time: e.time,
        total: ++total,
        closed: closed,
      })
    }

    else {
      points.push({
        time: e.time,
        total: total,
        closed: ++closed,
      })
    }
  })

  return [points, total]
}
