document.addEventListener('DOMContentLoaded', init)

function init() {
  var margin = {top: 20, right: 40, bottom: 40, left: 20 }
  // Width and height of the graph, not of the SVG
  var width = 900 - margin.left - margin.right
  var height = 400 - margin.top - margin.bottom

  var svg = d3.select("body").append("svg")
      .attr("width", width + margin.left + margin.right)
      .attr("height", height + margin.top + margin.bottom)
      .append("g")
      .attr("transform", `translate(${margin.left}, ${margin.top})`)

  // Nice colors provided by d3
  var c = d3.schemeCategory20

  d3.json("spacemacs-issues.json", function getData(err, data) {
    if (err) {
      console.error(err)
      return
    }

    var [points, total_issues, closed_issues] = prepare_data(data)
    var earliest = points[0].time

    // X and Y axes
    var x = d3.scaleTime()
        .domain([earliest, Date.now()])
        .range([0, width])

    var y = d3.scaleLinear()
        .domain([0, total_issues])
        .range([height, 0])   // reversed because SVG origin is top-left

    // Draw the axes
    svg.append("g")
      .attr("class", "x axis")
      .attr("transform", `translate(0, ${height})`)
      .call(d3.axisBottom(x))

    svg.append("g")
      .attr("class", "y axis")
      .attr("transform", `translate(${width}, 0)`)
      .call(d3.axisRight(y))

    // Draw the total (open+closed) area
    var total_issues_area = d3.area()
        .x(function(d) { return x(d.time) })
        .y1(function(d) { return y(d.total) })
        .y0(y(0))

    svg.datum(points)
      .append("path")
      .attr("d", total_issues_area)
      .attr("fill", c[7])

    // Draw the closed area
    var closed_issues_area = d3.area()
        .x(function(d) { return x(d.time) })
        .y1(function(d) { return y(d.closed) })
        .y0(y(0))

    svg.datum(points)
      .append("path")
      .attr("d", closed_issues_area)
      .attr("fill", c[0])

    // Second SVG that only displays the running total of opened issues
    // (open+closed - closed)

    // Adjust height
    height = 200 - margin.top - margin.bottom

    // Create SVG
    var open_issues_svg = d3.select("body").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", `translate(${margin.left}, ${margin.top})`)

    // Change Y axis
    y = d3.scaleLinear()
      .domain([0, total_issues - closed_issues])
      .range([height, 0])   // reversed because SVG origin is top-left

    // Draw the axes
    open_issues_svg.append("g")
      .attr("class", "x axis")
      .attr("transform", `translate(0, ${height})`)
      .call(d3.axisBottom(x))

    open_issues_svg.append("g")
      .attr("class", "y axis")
      .attr("transform", `translate(${width}, 0)`)
      .call(d3.axisRight(y).ticks(4))

    // Draw the area of number of open issues over time
    var open_issues_area = d3.area()
        .x(function(d) { return x(d.time) })
        .y1(function(d) { return y(d.total - d.closed) })
        .y0(y(0))

    open_issues_svg.datum(points)
      .append("path")
      .attr("d", open_issues_area)
      .attr("fill", c[7])
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

  return [points, total, closed]
}
