var Github_api = require("github")
var auth_token = require("./auth.json")["token"]

// Create the client
var github = new Github_api({
  protocol: "https",            // this might be the default, but hey

  // the docs mention you should set this, even though I use a token for auth so
  // they know who I am
  headers: {
    "user-agent": "fmdkdd/github-burndown-chart"
  },
})

// Authenticate using a token that's not in version control.
github.authenticate({
  type: "token",
  token: auth_token,
})

// These should be command-line parameters
var user = "syl20bnr"
var repo = "spacemacs"

// Start the request for the first page for results
var req = github.issues.getForRepo({
  user: user,
  repo: repo,
  state: "all",
  per_page: 100,                // we want /all/ the issues here, so might as
                                // well get the max per API call
}, getIssues)


// Collect the extracted issues in this object
var issues = []

function getIssues(err, res) {
  // Abort now if error
  if (err) {
    return false
  }

  // Discard most issue information.  We just want the dates, ids, and to know
  // if it's a pull request or not.
  res.forEach(issue => {
    issues.push({
      id: issue.id,
      number: issue.number,
      state: issue.state,
      pull_request: "pull_request" in issue,
      created_at: issue.created_at,
      closed_at: issue.closed_at,
    })
  })

  // Fetch the next page if it exists
  if (github.hasNextPage(res)) {
    github.getNextPage(res, getIssues)
  }
  // Otherwise write to output
  else {
    outputIssues()
  }
}

function outputIssues() {
  // Write all collected info to output as JSON
  console.log(JSON.stringify(issues))
}
