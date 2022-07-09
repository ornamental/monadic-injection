# Compile-Time Dependency Injection in Monadic Context

Applications frequently require values and resources created in monadic context to be used as dependencies in other
components which in their turn are allocated in such context.

This is a `shapeless`-based tool allowing one to organize the creation of dependencies and their eventual usage in an
effectful computation.

_Example_ (`@@`-tagged types are provied by `supertagged`):

```scala
final case class Configuration(
  port: Int @@ "service-port", 
  dataSourceUrl: URL @@ "data-source",
  pollingInterval: FiniteInterval @@ "data-source")

trait DataSourceClient[F[_]] { ... }

class DataSourceClientImpl[F[_]: Concurrent] private (
  sttpBackend: SttpBackend[F, Any],
  dataSourceUrl: URL,
  pollingInterval: FiniteInterval,
  requestCounter: Ref[F, RequestCounter]) extends DataSourceClient[F] { ... }

object DataSourceClientImpl {

  def apply[F[_]: Concurrent](
      sttpBackend: SttpBackend[F, Any],
      dataSourceUrl: URL @@ "data-source",
      pollingInterval: FiniteInterval @@ "data-source"): F[DataSourceClientImpl[F]] =
    Ref[F].of(RequestCounter.empty) >>=
      (new DataSourceClientImpl(sttpBackend, dataSourceUrl, pollingInterval, _))
}

object Service {

  def run[F[_]: Concurrent](
      port: Int @@ "service-port",
      dataSourceClient: DataSourceClient[F]): F[Nothing] = { ... }
}

object Main {

  private val configuration = Configuration(
    port = @@["service-port"](9000),
    dataSourceUrl = @@["data-source"](new URL("https://data-source:80")),
    pollingInterval = @@["data-source"](10.seconds))

  def run[F[_]: Concurrent]: F[Unit] =
    InjectionContext[F]()
      .provideFields(configuration)
      .provideR(AsyncHttpClientCatsBackend.resource[F]())
      .addF(DataSourceClientImpl[F](_, _, _))
      .use(Service.run(_, _))
}
```
