package com.example

import smithy4s.hello._
import cats.effect._
import cats.implicits._
import cats.effect.syntax.resource._
import org.http4s.ember.server._
import org.http4s.blaze.server._
import org.http4s._
import com.comcast.ip4s._
import smithy4s.http4s.SimpleRestJsonBuilder
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.client.Client
import org.http4s.server.middleware.{Logger => ServerLogger}
import org.http4s.client.middleware.{Logger => ClientLogger}

object HelloWorldImpl extends HelloWorldService[IO] {

  def hello(
      name: String,
      town: Option[String]
  ): IO[HelloOutput] =
    IO.pure { HelloOutput(mkMessage(name, town)) }

  def hello2(
      name: String,
      age: Option[Int],
      town: Option[String]
  ): IO[Hello2Output] =
    IO.pure { Hello2Output(mkMessage(name, town)) }

  def mkMessage(name: String, town: Option[String]): String =
    town.fold(s"Hello " + name + "!")(t =>
      s"Hello " + name + " from " + t + "!"
    )
}

object Routes {

  HttpRoutes.of[IO] {
    case _ => IO(Response[IO]().withEntity("Hello"))
  }

  private val example: Resource[IO, HttpRoutes[IO]] =
    SimpleRestJsonBuilder.routes(HelloWorldImpl).resource

  val all: Resource[IO, HttpRoutes[IO]] = example
}

object Main extends IOApp.Simple {
  // client middleware
  val authMiddleware: org.http4s.client.Middleware[IO] = { client =>
    Client { req =>
      client.run(
        req.withHeaders(
          headers.Authorization(Credentials.Token(AuthScheme.Bearer, "TOKEN"))
        )
      )
    }
  }

  val client = EmberClientBuilder.default[IO].build.map { client =>
    SimpleRestJsonBuilder(HelloWorldService)
      .clientResource(
        ClientLogger(
          true,
          true,
          logAction = Some((x: String) => IO.println(s"client: $x"))
        )(client),
        Uri.unsafeFromString("http://127.0.0.1:9000")
      )
  }

  // slow, ~ 35s on my M1
  def hello2Twice(hs: HelloWorldService[IO]): IO[Unit] = {
    hs.hello2("call 1").flatTap(IO.println) *>
      hs.hello2("call 2").flatTap(IO.println).void
  }

  // will fail
  def helloTwice(hs: HelloWorldService[IO]): IO[Unit] = {
    hs.hello("call 1").flatTap(IO.println) *>
      hs.hello("call 2").flatTap(IO.println).void
  }

  // will fail
  def helloThenHello2(hs: HelloWorldService[IO]): IO[Unit] = {
    hs.hello("call 1").flatTap(IO.println) *>
      hs.hello2("call 2").flatTap(IO.println).void
  }

  // will succeed
  def hello2ThenHello(hs: HelloWorldService[IO]): IO[Unit] = {
    hs.hello2("call 1").flatTap(IO.println) *>
      hs.hello("call 2").flatTap(IO.println).void
  }

  val run = (client, Routes.all).tupled.flatMap { case (client, routes) =>
    for {
      hs <- client
      _ <- BlazeServerBuilder
        .apply[IO]
        .bindLocal(9000)
        .withHttpApp(ServerLogger(true, true, cats.arrow.FunctionK.id[IO], logAction = Some((x:String) => IO.println(s"server: $x")))(routes.orNotFound))
        .resource
      // _ <- EmberServerBuilder
      //   .default[IO]
      //   .withPort(port"9000")
      //   .withHost(host"0.0.0.0")
      //   .withHttpApp(
      //     ServerLogger(
      //       true,
      //       true,
      //       cats.arrow.FunctionK.id[IO],
      //       logAction = Some((x: String) => IO.println(s"server: $x"))
      //     )(routes.orNotFound)
      //   )
      //   .build
      _ <- helloTwice(hs).toResource
      // _ <- hello2Twice(hs).toResource
      // _ <- helloThenHello2(hs).toResource
      // _ <- hello2ThenHello(hs).toResource
    } yield ()

  }.use_

}
