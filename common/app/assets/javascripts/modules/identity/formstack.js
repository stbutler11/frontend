define([
    'common/$',
    'common/common',
    'bean',
    'common/modules/identity/api'
], function (
    $,
    common,
    bean,
    idApi
) {

    function Formstack(el, formstackId, context, config) {

        var self = this,
            dom = {},
            formId = formstackId.split('-')[0];

        config = common.extend({
            idClasses: {
                form: 'form',
                field: 'form-field',
                note: 'form-field__note form-field__note--below',
                label: 'label',
                textInput: 'text-input',
                textArea: 'textarea textarea--no-resize',
                submit: 'submit-input',
                fieldError: 'form-field--error',
                formError: 'form__error',
                required: 'formstack-required',
                sectionHeader: 'formstack-heading',
                sectionHeaderFirst: 'formstack-heading--first',
                sectionText: 'formstack-section',
                hide: 'is-hidden'
            },
            fsSelectors: {
                form: '#fsForm' + formId,
                field: '.fsRow',
                note: '.fsSupporting, .showMobile',
                label: '.fsLabel',
                textInput: '.fsField[type="text"], .fsField[type="email"], .fsField[type="number"], .fsField[type="tel"]',
                textArea: 'textarea.fsField',
                submit: '.fsSubmitButton',
                fieldError: '.fsValidationError',
                formError: '.fsError',
                required: '.fsRequiredMarker',
                sectionHeader: '.fsSectionHeading',
                sectionHeaderFirst: '.fsSection:first-child .fsSectionHeading',
                sectionText: '.fsSectionText',
                hide: '.hidden, .fsHidden'
            },
            hiddenSelectors: {
                userId: '[type="number"]',
                email: '[type="email"]'
            }
        }, config);

        self.init = function() {
            // User object required to populate fields
            var user = idApi.getUserOrSignIn();

            self.dom(user);
            $(el).removeClass(config.idClasses.hide);

            // Update iframe height, see "modules/identity/formstack-iframe"
            self.postMessage('ready');
        };

        self.dom = function(user) {
            // Formstack generates some awful HTML, so we'll remove the CSS links,
            // loop their selectors and add our own classes instead
            dom.$form = $(config.fsSelectors.form).addClass(config.idClasses.form);
            $('link', el).remove();

            for (var selector in config.fsSelectors) {
                $(config.fsSelectors[selector], dom.$form).addClass(config.idClasses[selector]);
            }

            // Formstack also don't have capturable hidden fields,
            // so we remove ID text inputs and append hidden equivalents
            var $userId = $(config.hiddenSelectors.userId, dom.$form).remove(),
                $email = $(config.hiddenSelectors.email, dom.$form).remove(),

                html = '<input type="hidden" name="' + $userId.attr('name') + '" value="' + user.id + '">'
                     + '<input type="hidden" name="' + $email.attr('name') + '" value="' + user.primaryEmailAddress + '">';

            dom.$form.append(html);

            // Events
            bean.on(window, 'unload', self.unload);
            bean.on(dom.$form[0], 'submit', self.submit);
        };

        self.submit = function(event) {
            // TODO: FML
            setTimeout(function() {
                // Remove any existing errors
                $('.' + config.idClasses.formError).removeClass(config.idClasses.formError);
                $('.' + config.idClasses.fieldError).removeClass(config.idClasses.fieldError);

                // Handle new errors
                $(config.fsSelectors.formError, dom.$form).addClass(config.idClasses.formError);
                $(config.fsSelectors.fieldError, dom.$form).addClass(config.idClasses.fieldError);

                self.postMessage('refreshHeight');
            }, 100);
        };

        self.unload = function(event) {
            // Listen for navigation to success page
            self.postMessage('unload');
        };

        self.postMessage = function(message) {
            var domain = config.page.idUrl;
            window.top.postMessage(message, domain);
        };

    }

    return Formstack;

});
