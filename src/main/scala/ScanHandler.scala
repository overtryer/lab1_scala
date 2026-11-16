class ScanHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.sources.isEmpty then
          ctx.ui.println("Сначала добавьте источники.")
        else
          ctx.ui.println("Сканирование...")
          ctx.logger.info("Начало сканирования изменений")

          val report = BusinessLogic.scan(session, ctx.scanner)
          BusinessLogic.scanLines(report).foreach(ctx.ui.println)

          if report.stats.isEmpty && report.problems.isEmpty then
            ctx.logger.info("Сканирование завершено. Изменений нет.")
          else
            ctx.logger.info(s"Сканирование завершено. Изменения: +${report.stats.added}, ~${report.stats.modified}, -${report.stats.removed}, пер.: ${report.stats.renamed}")
        state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state