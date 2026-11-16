import scala.annotation.tailrec

object App:

  def run(config: AppConfig, logger: Logger, ui: Ui): Unit =
    val scanner = new Scanner(config.hashAlgorithm)
    val ctx = AppContext(config, logger, ui, new ZipStorageService, scanner)
    val handlers: Map[Command, CommandHandler] = Map(
      Command.Info -> new InfoHandler,
      Command.Versions -> new VersionsHandler,
      Command.Scan -> new ScanHandler,
      Command.Reload -> new ReloadHandler,
      Command.Sources -> new SourcesHandler,
      Command.Open -> new OpenHandler,
      Command.Create -> new CreateHandler,
      Command.Verify -> new VerifyHandler,
      Command.Restore -> new RestoreHandler,
      Command.Backup -> new BackupHandler
    )
    
    @tailrec
    def loop(state: AppState): Unit =
      printMenu(ui)
      ui.readLine() match
        case None =>
          ()

        case Some(line) =>
          Command.parse(line) match
            case Left(err) =>
              val msg = ParseErrorRender.render(err)
              ui.println(msg)
              logger.warn(s"Некорректный ввод: $msg")
              loop(state)

            case Right(Command.Exit) =>
              ui.println("До свидания.")
              ()

            case Right(cmd) =>
              val nextState = handleCommand(cmd, state, ctx, handlers)
              loop(nextState)
    
    loop(AppState.NoStorage)

  private def handleCommand(cmd: Command, state: AppState, ctx: AppContext, handlers: Map[Command, CommandHandler]): AppState =
    handlers.get(cmd) match
      case Some(handler) => handler.execute(state, ctx)
      case None =>
        ctx.ui.println("Команда не реализована.")
        ctx.logger.warn("Команда не реализована")
        state