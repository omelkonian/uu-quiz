
var postQuiz = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/quiz'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getQuizByQuizId = function(quizId, onSuccess, onError)
{
  $.ajax(
    { url: '/quiz/' + encodeURIComponent(quizId) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postQuestion = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/question'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getQuestionByQuizIdByOrder = function(quizId, order, onSuccess, onError)
{
  $.ajax(
    { url: '/question/' + encodeURIComponent(quizId) + '/' + encodeURIComponent(order) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postMultipleChoice = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/multipleChoice'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getMultipleChoiceByQuestionIdByOrder = function(questionId, order, onSuccess, onError)
{
  $.ajax(
    { url: '/multipleChoice/' + encodeURIComponent(questionId) + '/' + encodeURIComponent(order) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postOpenText = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/openText'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getOpenTextByQuestionId = function(questionId, onSuccess, onError)
{
  $.ajax(
    { url: '/openText/' + encodeURIComponent(questionId) + ''
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var getQuizIds = function(onSuccess, onError)
{
  $.ajax(
    { url: '/quizIds'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
