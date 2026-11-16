class InfoHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        BusinessLogic.infoLines(session).foreach(ctx.ui.println)
        state
      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state