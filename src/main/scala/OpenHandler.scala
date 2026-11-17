import java.nio.file.Paths

class OpenHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    ctx.logger.info("Попытка открытия хранилища")
    ctx.ui.println("Введите путь к файлу хранилища:")
    val path = Paths.get(ctx.ui.readLineTrimmed())
    
    BusinessLogic.openSession(ctx.storage, path) match
      case Right(sessionWithoutKey) =>
        if sessionWithoutKey.encrypted then
          ctx.ui.println("Введите ключ доступа:")
          val keyInput = ctx.ui.readLineTrimmed()
          
          BusinessLogic.openEncryptedSession(ctx.storage, path, keyInput) match
            case Right(finalSession) =>
              ctx.ui.println(s"Хранилище '${finalSession.name}' открыто.")
              ctx.logger.info(s"Хранилище успешно открыто: ${finalSession.name}")
              AppState.Opened(finalSession)
            case Left(err) =>
              ctx.ui.println(s"Ошибка: ${err.message}")
              ctx.logger.warn(s"Не удалось открыть зашифрованное хранилище: ${err.message}")
              state
        else
          ctx.ui.println(s"Хранилище '${sessionWithoutKey.name}' открыто.")
          ctx.logger.info(s"Хранилище успешно открыто: ${sessionWithoutKey.name}")
          AppState.Opened(sessionWithoutKey)

      case Left(err) =>
        ctx.ui.println(s"Ошибка чтения: ${err.message}")
        ctx.logger.error(s"Не удалось открыть хранилище: ${err.message}")
        state