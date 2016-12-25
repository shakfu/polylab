
#' Constructor
EmailClass <- function(name, email) {
	nc = list(
		name = name,
		email = email,
		get = function(x) nc[[x]],
		set = function(x, value) nc[[x]] <<- value,
		props = list(),
		history = list(),
		getHistory = function() return(nc$history),
		getNumMessagesSent = function() return(length(nc$history))
	)
	#Add a few more methods
	nc$sendMail = function(to) {
		cat(paste("Sending mail to", to, 'from', nc$email))
		h <- nc$history
		h[[(length(h)+1)]] <- list(to=to, timestamp=Sys.time())
		assign('history', h, envir=nc)
	}
	nc$addProp = function(name, value) {
		p <- nc$props
		p[[name]] <- value
		assign('props', p, envir=nc)
	}
	nc <- list2env(nc)
	class(nc) <- "EmailClass"
	return(nc)
}

#' Define S3 generic method for the print function.
print.EmailClass <- function(x) {
	if(class(x) != "EmailClass") stop();
	cat(paste(x$get("name"), "'s email address is ", x$get("email"), sep=''))
}

if(FALSE) {  #Test code that won't be run when sourcing this file
	test <- EmailClass(name="Jason", "jason@bryer.org")
	test$addProp('hello', 'world')
	test$props
	test
	class(test)
	str(test)
	test$get("name")
	test$get("email")
	test$set("name", "Heather")
	test$get("name")
	test
	test$sendMail("jbryer@excelsior.edu")
	test$getHistory()
	test$sendMail("test@domain.edu")
	test$getNumMessagesSent()
	
	test2 <- EmailClass("Nobody", "dontemailme@nowhere.com")
	test2
	test2$props
	test2$getHistory()
	test2$sendMail('nobody@exclesior.edu')
}
