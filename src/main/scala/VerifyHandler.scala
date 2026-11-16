class VerifyHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        ctx.ui.println("Проверка целостности...")
        ctx.logger.info("Начало проверки целостности")
        
        val errors = BusinessLogic.verifyFiles(ctx.storage, session, ctx.scanner)

        if errors.isEmpty then
          ctx.ui.println(s"Целостность в порядке. Проверено: ${session.manifest.files.size}")
          ctx.logger.info(s"Проверка целостности пройдена. Проверено: ${session.manifest.files.size}")
        else
          errors.foreach { msg =>
            ctx.ui.println(msg)
            ctx.logger.error(msg)
          }
          ctx.ui.println(s"Ошибок: ${errors.size} из ${session.manifest.files.size}")
          ctx.logger.error(s"Проверка целостности завершилась с ${errors.size} ошибками")
        state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state