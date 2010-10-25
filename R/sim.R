sim <- function(obj, x=NULL, x1=NULL, num=1000, prev=NULL,
                bootstrap=F, boot.fn=NULL, cond.data=NULL,
                ...) {
  UseMethod("sim")
}
