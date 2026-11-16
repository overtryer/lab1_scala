import java.nio.file.Paths
import java.time.Instant

class SourcesHandler extends CommandHandler:
  def execute(state: AppState, ctx: AppContext): AppState =
    state match
      case AppState.Opened(session) =>
        ctx.ui.println("Текущие источники:")
        session.sources.zipWithIndex.foreach { case (s, idx) =>
          val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
          ctx.ui.println(s"${idx + 1}. ${s.path} [$status]")
          ctx.ui.println(s"   Последняя копия: ${s.lastBackup.map(formatMinute).getOrElse("—")}")
        }

        ctx.ui.println("\nУправление:")
        ctx.ui.println("1. Добавить")
        ctx.ui.println("2. Отключить/Включить")
        ctx.ui.println("3. Удалить")
        ctx.ui.println("4. Изменить настройки")
        ctx.ui.println("0. Назад")
        ctx.ui.print("Выбор: ")

        val choice = ctx.ui.readLineTrimmed()

        val actionEither: Either[ParseError, SourceAction] =
          SourceAction.parseKind(choice).flatMap {
            case SourceAction.Kind.Add  => Right(SourceAction.Add)
            case SourceAction.Kind.Back => Right(SourceAction.Back)
            case kind =>
              ctx.ui.print("Введите номер источника: ")
              SourceAction.parseIndex(ctx.ui.readLineTrimmed(), session.sources.size)
                .map(idx => SourceAction.combine(kind, idx))
          }

        actionEither match
          case Left(parseErr) =>
            ctx.ui.println(ParseErrorRender.render(parseErr))
            state

          case Right(SourceAction.Back) =>
            state

          case Right(action) =>
            val updatedSessionEither: Either[AppError, StorageSession] = action match
              case SourceAction.Add =>
                ctx.ui.println("Введите путь к папке:")
                val p = Paths.get(ctx.ui.readLineTrimmed())
                BusinessLogic.addSource(session, p)

              case SourceAction.Toggle(idx) =>
                BusinessLogic.toggleSource(session, idx)

              case SourceAction.Remove(idx) =>
                BusinessLogic.removeSource(session, idx)

              case SourceAction.Edit(idx) =>
                val src = session.sources(idx - 1)
                ctx.ui.println(s"Включать вложенные каталоги? (y/n) (сейчас: ${src.recursive}):")
                val newRec = ctx.ui.readLineTrimmed().toLowerCase == "y"
                ctx.ui.println(s"Исключения через запятую (сейчас: ${src.exclusions.map(_.pattern).mkString(", ")}):")
                val exStr = ctx.ui.readLineTrimmed()
                val newExclusions = if exStr.isEmpty then Nil else exStr.split(",").map(s => Glob(s.trim)).toList
                BusinessLogic.editSource(session, idx, newRec, newExclusions)

              case SourceAction.Back => Right(session)

            updatedSessionEither match
              case Left(err) =>
                ctx.ui.println(s"Ошибка: ${err.message}")
                state
              case Right(updated) =>
                if updated.sources != session.sources then
                  ctx.storage.saveSession(updated, Map.empty, session.key, ctx.config) match
                    case Right(_) =>
                      ctx.ui.println("Источники обновлены.")
                      ctx.logger.info("Конфигурация источников сохранена")
                      updated.sources.zipWithIndex.foreach { case (s, idx) =>
                        val status = if s.enabled then "включен" else "ОТКЛЮЧЕН"
                        ctx.ui.println(s"  ${idx + 1}. ${s.path} [$status]")
                      }
                    case Left(err) =>
                      ctx.ui.println(s"Ошибка сохранения: ${err.message}")
                      ctx.logger.error(s"Не удалось сохранить источники: ${err.message}")
                AppState.Opened(updated)

      case AppState.NoStorage =>
        ctx.ui.println("Сначала откройте или создайте хранилище.")
        ctx.logger.warn("Команда отклонена: хранилище не открыто")
        state

  private def formatMinute(i: Instant): String = i.toString.take(16).replace("T", " ")