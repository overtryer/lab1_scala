import java.nio.file.Paths

class ReloadHandler extends CommandHandler:
  private val configPath = Paths.get("src/main/scala/config.json")

  def execute(state: AppState, ctx: AppContext): AppState =
    ctx.ui.println("Перезагрузка конфигурации...")
    BusinessLogic.reload(configPath) match
      case Right((newCfg, newScanner)) =>
        ctx.config = newCfg
        ctx.scanner = newScanner
        ctx.ui.println("Конфигурация перезагружена и применена.")
        ctx.logger.info("Конфигурация перезагружена и применена")
        state
      case Left(err) =>
        ctx.ui.println(s"Ошибка перезагрузки: ${err.message}")
        ctx.logger.error(s"Не удалось перезагрузить конфигурацию: ${err.message}")
        state