# A RESTful decentralized gradebook

"Progress" is a pairing between worksheets and learners.

A chart is promising to store progress for all its users, and lists
of worksheets for its courses.

# What is a worksheet?

A worksheet is represented by a URI.

The hostname in the URI must agree with the
`window.location.hostname`.

Generally the worksheet URI is sent in the `X-Worksheet` header, and
then referred to later via its sha256.  The worksheet URI must be
normalized by applying [normalizations that preserve
semantics](https://en.wikipedia.org/wiki/URI_normalization).

# What is a user?  What is a learner?

A learner is a user.

A user is identified by `username@chart` where `username` is randomly
chosen.

A JWT identifies a user, and a scope for their possible actions.

Courses are owned by a user.

A single person might be represented by multiple identities on doenet.

# How is decentralized identity handled? 

At various points, a user will need to prove their identity to another
server.

For example, an instructor must prove their identity to a user's
chart; this then means that the instructor is entitled to the
learner's data.  This is done by providing a JWT using a public key.
The public key can be verified with the other chart, ultimately
protected via TLS.

# What is a course?

Like a user, a course is also identified as `name@chart`.

A course includes worksheets.

A course includes a list of enrolled learners as well as instructional
staff.

# How does hashcash work?

Various endpoints require a payment of hashcash, which is provided as
an `X-Hashcash` header.  The hashcash is formatted as a JSON Work
Proof.

# What about people who aren't logged in?

When POSTing to `/learners/me/worksheets/:worksheet/progress`, it is
possible that there is no JWT cookie.  In this case, we would like to
manufacture a "guest" user (with no password...) and set the cookie.
But third party cookies won't be sent anyway, so there is not much
need to do this.

A guest user has no ability to log in (e.g., a guest has no password!)
and depends entirely on the JWT token as proof-of-identity.

## LTI 1.3 support

### GET /lti/launch
### POST /lti/launch

This endpoint handles the LTI launch, as described by [5.1.1.1 Step 1:
Third-party Initiated
Login](http://www.imsglobal.org/spec/security/v1p0/#step-1-third-party-initiated-login).
And as the [LTI 1.3
specification](http://www.imsglobal.org/spec/security/v1p0/) says, the
"redirect may be a form POST or a GET - a Tool must support either
case".

### GET /lti/auth 

This completes the OIDC flow as described by [5.1.1.3 Step 3: Authentication Response](http://www.imsglobal.org/spec/security/v1p0/#step-3-authentication-response).

## Users

### POST /users/:email

Create a new user, sending a password to the given email

TODO: how does this handle multiple accounts?

### GET /users/:user

Get information about a user.

### PUT /users/:user
### PATCH /users/:user

Update a user.

### GET /users/:user/token

Log in as the given user.  Password is sent in the `Authorization:
Basic` header.  Responds by returning a token in the body containing a
JWT.

### GET /users/:user/charts/:chart/token

As a user on this chart, get a JWT scoped for the given chart.

### GET /users/:user/authorize

Log in as the given user.  Password is sent in the `Authorization:
Basic` header.  Responds by setting a cookie containing a JWT.

## Learners, progress, page state, statements

### POST /worksheets/:worksheet/token

Redirect to the given worksheet, passing the worksheet a symmetric JWT
token in the query string (or hash?) narrowly scoped to the
worksheet's domain to permit progress and do state updates.

The worksheet URL is stored in a POST variable.

### PUT /learners/:user/worksheets/:worksheet/progress

Record progress on this worksheet.

Hashcash required.

### GET /learners/:user/worksheets/:worksheet/progress

Retrieve progress on this worksheet.

### GET /learners/:user/worksheets/:worksheet/state

Retrieve page state on this worksheet.

### PATCH /learners/:user/worksheets/:worksheet/state
### PUT /learners/:user/worksheets/:worksheet/state

Record or update page state on this worksheet.

Hashcash required.

### POST /learners/:user/worksheets/:worksheet/statements

Record a learner event (meaning an xAPI statement) for the given
worksheet.

### GET /learners/:user/worksheets/:worksheet/statements
### GET /learners/:user/statements
### GET /statements/:id

Get a specific learner event.

### GET /learners/:user/statements/recent

Get (an unspecified number of) recent events for the given learner.

## Courses

### POST /courses/:course
### DELETE /courses/:course
### PUT /courses/:course
### PATCH /courses/:course
### GET /courses/:course

Create or delete a course; the current user becomes an "instructor"
when creating a course.

This requires hashcash.

### GET /courses/:course/instructors

Get a list of instructors in a course.

### POST /courses/:course/instructors/:user

Add an instructor in a course; only an instructor is permitted to add
other instructors.

### DELETE /courses/:course/instructors/:user

Remove an instructor from a course.  The final instructor cannot be
removed.


### GET /courses/:course/learners

Get a list of learners enrolled in a course.

### GET /learners/:user/courses

Get a list of all courses a learner is enrolled in.

### GET /instructors/:user/courses

Get a list of all courses an instructor is teaching.

### DELETE /courses/:course/learners/:user

Disenroll a student from the course.

### POST /courses/:course/learners/:user

Enroll a student in a course; students can enroll themselves in a course.

### GET /courses/:course/learners/:user

Retrieve information about the user as a learner in the given course.

### GET /courses/:course/progress

Get a list of scores for all the learners and worksheets.

## Worksheets and the "gradebook"

### POST /worksheets

Create a new worksheet

### GET /courses/:course/assignments

View all the assignments for the course.

If the course is not homed at this chart, a redirect is provided.

### GET /courses/:course/assignments/:assignment
### PUT /courses/:course/assignments/:assignment
### POST /courses/:course/assignments/:assignment
### DELETE /courses/:course/assignments/:assignment

add or update or delete a worksheet to a course

assignments have deadlines (and exceptions)

# hub.doenet.cloud

static site
