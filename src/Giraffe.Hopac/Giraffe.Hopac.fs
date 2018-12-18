namespace Giraffe.Hopac

open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging
open Giraffe.Core
open Hopac

[<AutoOpen>]
module Core =
    /// ** Description **
    /// A type alias for `Job<HttpContext option>` which represents the result of a HTTP function (`HttpFunc`).
    /// If the result is `Some HttpContext` then the Giraffe middleware will return the response to the client and end the pipeline. However, if the result is `None` then the Giraffe middleware will continue the ASP.NET Core pipeline by invoking the `next` middleware.
    type HttpFuncResultJ = Job<HttpContext option>

    let ofHttpFuncResult (httpFuncResult : HttpFuncResult) : HttpFuncResultJ = Job.fromTask(fun () -> httpFuncResult)
    let toHttpFuncResult (httpFuncResult : HttpFuncResultJ) : HttpFuncResult = httpFuncResult |> startAsTask

    /// ** Description **
    /// A HTTP function which takes an `HttpContext` object and returns a `HttpFuncResultJ`.
    /// The function may inspect the incoming `HttpRequest` and make modifications to the `HttpResponse` before returning a `HttpFuncResult`. The result can be either a `Job` of `Some HttpContext` or a `Job` of `None`.
    /// If the result is `Some HttpContext` then the Giraffe middleware will return the response to the client and end the pipeline. However, if the result is `None` then the Giraffe middleware will continue the ASP.NET Core pipeline by invoking the `next` middleware.
    type HttpFuncJ = HttpContext -> HttpFuncResultJ

    let ofHttpFunc (httpFunc : HttpFunc) : HttpFuncJ = httpFunc >> ofHttpFuncResult
    let toHttpFunc (httpFunc : HttpFuncJ) : HttpFunc = httpFunc >> toHttpFuncResult

    /// ** Description **
    /// A HTTP handler is the core building block of a Giraffe web application. It works similarily to ASP.NET Core's middleware where it is self responsible for invoking the next `HttpFunc` function of the pipeline or shortcircuit the execution by directly returning a `Job` of `HttpContext option`.
    type HttpHandlerJ = HttpFuncJ -> HttpFuncJ
    
    let ofHttpHandler (httpHandler : HttpHandler) : HttpHandlerJ  =
        toHttpFunc >> httpHandler >> ofHttpFunc
    let toHttpHandler (httpHandler : HttpHandlerJ) : HttpHandler  =
        ofHttpFunc >> httpHandler >> toHttpFunc


    /// ** Description **
    /// The error handler function takes an `Exception` object as well as an `ILogger` instance and returns a `HttpHandler` function which takes care of handling any uncaught application errors.
    type ErrorHandlerJ = exn -> ILogger -> HttpHandlerJ

    let composeJ (handler1 : HttpHandlerJ) (handler2 : HttpHandlerJ) : HttpHandlerJ =
        fun (final : HttpFuncJ) ->
            let func = final |> handler2 |> handler1
            fun (ctx : HttpContext) ->
                match ctx.Response.HasStarted with
                | true  -> final ctx
                | false -> func ctx

    let (>==>) = composeJ
    let (>-=>) (handler1 : HttpHandler) (handler2 : HttpHandlerJ) : HttpHandlerJ = (handler1 |> ofHttpHandler) >==> handler2
    let (>=->) (handler1 : HttpHandlerJ) (handler2 : HttpHandler) : HttpHandlerJ = (handler1) >==> (handler2 |> ofHttpHandler)
    let (>-->) (handler1 : HttpHandler) (handler2 : HttpHandler) : HttpHandlerJ = (handler1 |> ofHttpHandler) >==> (handler2 |> ofHttpHandler)
    let (>==>-) (handler1 : HttpHandlerJ) (handler2 : HttpHandlerJ) : HttpHandler  = (handler1 >==> handler2) |> toHttpHandler
    let (>-=>-) (handler1 : HttpHandler) (handler2 : HttpHandlerJ) : HttpHandler = (handler1) >=> (handler2 |> toHttpHandler)
    let (>=->-) (handler1 : HttpHandlerJ) (handler2 : HttpHandler) : HttpHandler = (handler1 |> toHttpHandler ) >=> (handler2)
