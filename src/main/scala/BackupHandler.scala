class BackupHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        if session.sources.isEmpty then
          ctx.ui.println("Нет источников.")
          state
        else
          ctx.ui.println("Сканирование перед копированием...")
          ctx.logger.info("Начало резервного копирования")
          val result = ctx.scanner.scan(session.sources)
          val changes = Differ.diff(session.manifest, result, session.sources)
          val stats = ChangeStats.from(changes)

          if stats.isEmpty && result.problems.isEmpty then
            ctx.ui.println("Нет новых данных для сохранения.")
            ctx.logger.info("Копирование отменено. Нет изменений.")
            state
          else
            ctx.ui.println(s"Обнаружено: +${stats.added}, ~${stats.modified}, -${stats.removed}, пер.: ${stats.renamed}")

            BusinessLogic.getUsableSpace(session.storageFile) match
              case Left(err) =>
                ctx.ui.println(s"Ошибка проверки свободного места: ${err.message}")
                ctx.logger.error(s"Ошибка проверки свободного места: ${err.message}")
                state
              case Right(usableSpace) if stats.newBytes > usableSpace =>
                ctx.ui.println("Недостаточно свободного места на диске.")
                ctx.logger.error("Ошибка копирования: недостаточно места на диске")
                state
              case Right(_) =>
                if ctx.config.confirmBeforeBackup then
                  ctx.ui.print("Продолжить? (y/n): ")
                  if ctx.ui.readLineTrimmed().toLowerCase != "y" then
                    ctx.logger.info("Копирование отменено пользователем")
                    return state
                
                val plan = BusinessLogic.planBackup(session, changes, result.problems)

                ctx.ui.println("Запись данных...")
                ctx.storage.saveSession(plan.newSession, plan.filesToAdd, session.key, ctx.config) match
                  case Right(_) =>
                    val successCount = plan.stats.added + plan.stats.modified + plan.stats.renamed
                    val failCount = result.problems.size
                    ctx.ui.println(s"Резервное копирование завершено. Версия: ${plan.newSession.manifest.version.value}")
                    ctx.logger.info(s"Копирование завершено. Версия: ${plan.newSession.manifest.version.value}. Успешно: $successCount, Проблемных: $failCount")
                    AppState.Opened(plan.newSession)
                  case Left(err) =>
                    ctx.ui.println(s"Ошибка записи: ${err.message}")
                    ctx.logger.error(s"Ошибка копирования: ${err.message}")
                    state

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state