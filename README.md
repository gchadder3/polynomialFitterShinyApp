# polynomialFitterShinyApp Repository

This project was done for the Developing Data Products course which is course 9 
of the Coursera Data Science Specialization, in which we were asked to develop 
and document a Shiny app of our own devising.  The repo contains R code in two 
different but equivalent forms for a Shiny app that allows you to do polynomial 
curve fitting on data passed to the app via a .csv file.  

Basic documentation and a walk-through of the app's functionality are at the 
project webage at:
https://gchadder3.github.io/polynomialFitterShinyApp/polyfitter.html

The alternate code implementations are:

* `app.R`: a single-file implementation of the app (the newest way of doing 
Shiny apps)
* `ui.R` and `server.R`: an old-style two-file implememtation of the same 
app

Some demo data (to upload to the app) is at:
https://github.com/gchadder3/polynomialFitterShinyApp/raw/master/demofits.csv